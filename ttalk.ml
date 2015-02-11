
type state = {
  headers: string array;
  bodies: string array;
  curr_step: int;
}

let data = ref None

let is_some = function None -> false | _ -> true
let is_none = function None -> true | _ -> false

let get_option option =
  assert (is_some option);
  match option with
    None -> assert false
  | Some x -> x

let root = (let loc = Dom_html.window##location in
			let s = Js.to_string in
			s(loc##protocol) ^ "//" ^ s(loc##host) ^ s(loc##pathname))

let instructions = Array.of_list [
  "Step 1 of 4: You have the first word. Craft a header and a body. You can revise them once, after your partner responds.";
  "Step 2 of 4: Your partner's draft is below. Now draft a response.
 You can revise it once (step 4), after your partner revises (step 3).
 You get the last word.";
  "Step 3 of 4: Make your revisions, taking into account your partner's response.";
  "Step 4 of 4: Your partner is done. Revise your response into your last word, taking into account your partner's finished word.";
  "This is a dialog. You can share the url. "
]

let get_data (): state =
  begin
	if !data = None then
	  let str = (Dom_html.window##location##search |> Js.to_string) in
	  if String.length str > 0 then
		begin
		  Printf.printf "str found"; flush_all();
 		  let str =
			StringLabels.sub str ~pos:1 ~len:(String.length str - 1) in
		  Printf.printf "%s" str;flush_all();
		  data := Some ((Marshal.from_string (B64.decode ~alphabet:B64.uri_safe_alphabet str) 0): state)
		end
	  else
		data := Some {
		  headers = Array.make 4 "";
		  bodies = Array.make 4 "";
		  curr_step = 1;
		}
  end;
  get_option !data

let set_data (state : state) =
  data := Some state  

let get_step () = let st = get_data () in st.curr_step

let doc = Dom_html.document

exception Bad_step

let get_coerced id coercion =
  Js.Opt.get
	(coercion (Dom_html.getElementById id))
	(fun () -> assert false)

let get_input id =
  get_coerced id Dom_html.CoerceTo.input

let get_textarea id =
  get_coerced id Dom_html.CoerceTo.textarea

let add_text elt str =
  Dom.appendChild elt (doc##createTextNode (Js.string str))

let delete_contents elt = elt##innerHTML <- Js.string ""

let message str =
  let msgElt = Dom_html.getElementById "message" in
  delete_contents msgElt;
  add_text msgElt str;
  msgElt##scrollIntoView (Js._true)

let get_header state step =
	state.headers.(step - 1)

let get_body state step =
	state.bodies.(step - 1)

let set_header state txt =
  state.headers.(state.curr_step - 1) <- txt

let set_body state txt =
  state.bodies.(state.curr_step - 1) <- txt

let get_instructions step =
	instructions.(step - 1)

let show_instructions state =
  let step = state.curr_step in
  let instructions = Dom_html.getElementById "instructions" in
  add_text instructions (get_instructions step)

let show_elt elt display =
  elt##style##display <- Js.string display

let hide_elt elt  =
  elt##style##display <- Js.string "none"

let hide_instructions () =
  let instructions = Dom_html.getElementById "instructions" in
  hide_elt instructions

let show_previous_pane state =
  match state.curr_step with 2 | 3 | 4 ->
	let previous_pane = Dom_html.getElementById "previous_pane" in
	show_elt previous_pane "block";
	let h = Dom_html.getElementById "previous_header" in
	add_text h (state.curr_step - 1 |> (get_header state));
	let body = Dom_html.getElementById "previous_body" in
	add_text body (state.curr_step -1 |> (get_body state))
  | 1 | 5 -> ()
  | _ -> raise Bad_step

let remaining elt max =
  max - (elt##value |> Js.to_string |> String.length)

let start_counting elt counter num =
  ignore(Lwt_js_events.keyups elt (fun _ _ ->
	delete_contents counter;
	add_text counter (Printf.sprintf "(Max Characters remaining: %d)" (remaining elt num));
	Lwt.return ()))

let header_max = 80
let body_max = 600

let within_limits () =
  let header = get_input "header" in
  let body = get_textarea "body" in
  (remaining header header_max) >= 0 &&
  (remaining body body_max) >= 0

let show_first_draft state =
  let header = get_input "header" in
  let body = get_textarea "body" in
  header##value <- get_header state (state.curr_step - 2) |> Js.string;
  body##value <- get_body state (state.curr_step - 2) |> Js.string

let show_input_pane state =
  match state.curr_step with 1 | 2 | 3 | 4 ->
	let input_pane = Dom_html.getElementById "input_pane" in
	show_elt input_pane "block";
	let header = get_input "header" in
	let body = get_textarea "body" in
	let header_counter = Dom_html.getElementById "header_counter" in
	let body_counter = Dom_html.getElementById "body_counter" in
	start_counting header header_counter header_max;
	start_counting body body_counter body_max
  | 5->()
  | _ -> raise Bad_step

let show_final_pane state =
  let header1 = Dom_html.getElementById "final_header_1" in
  let body1 = Dom_html.getElementById "final_body_1" in
  let header2 = Dom_html.getElementById "final_header_2" in
  let body2 = Dom_html.getElementById "final_body_2" in
  add_text header1 (get_header state 3);
  add_text body1 (get_body state 3);
  add_text header2 (get_header state 4);
  add_text body2 (get_body state 4);
  let link_elt = Dom_html.getElementById "link_to_new" in
  link_elt##setAttribute (Js.string "href", Js.string root);
  show_elt (Dom_html.getElementById "final_pane") "block"

let show_editing state =
  Printf.printf "step: %d" state.curr_step;flush_all();
  show_instructions state;
  show_input_pane state;
  show_previous_pane state;
  let get_next_url () =
	let state = get_data () in
	let header_elt = get_input "header" in
	set_header state (Js.to_string header_elt##value);
	let body_elt = get_textarea "body" in
	set_body state (Js.to_string body_elt##value);
	set_data ({ state with curr_step = state.curr_step + 1 });
	let str =
	  (Marshal.to_string (get_data (): state) []) |>
		  B64.encode ~alphabet:B64.uri_safe_alphabet in
	(root ^ "?" ^ str)
  in
  let update () =
	let url = get_next_url () in
	let copybox = get_input ("copybox") in
	copybox##value <- (Js.string url);
	ignore(Lwt_js_events.clicks copybox (fun _ _ ->
	  Lwt.return copybox##select ()));
	show_elt copybox "inline";
	hide_instructions ()
  in
  let update_button btn_txt message_txt update_fn =
	let btn = Dom_html.getElementById "input_button" in
	Dom.appendChild btn (doc##createTextNode (btn_txt |> Js.string));
	ignore(Lwt_js_events.clicks btn (fun _ _ ->
	  match within_limits () with
		true ->
		  begin
			update_fn ();
			match message_txt with
			  None -> Lwt.return_unit
			| Some message_txt ->
			  Lwt.return (message message_txt)
		  end
	  | false -> Lwt.return (message "Too much information! Boring!")))
  in
  begin
	let show_final_url () =
	  let url = get_next_url () in
	  Dom_html.window##location##replace (Js.string url)
	in
	match state.curr_step with 1|2|3 ->
	  update_button "Done" (Some "Ok, Copy this Url and send it to your partner") update
	| 4 ->
	  update_button "All Done" None show_final_url
	| 5 -> ();
	| _ -> raise Bad_step
  end;
  begin
	match state.curr_step with 3|4 ->
	  show_first_draft state
	| 1|2|5 -> ()
	| _ -> raise Bad_step
  end;	
  begin
	match state.curr_step with 1|2|3|4 -> ()
	| 5 ->
	  show_final_pane state
	| _ -> raise Bad_step
  end


let main () =
  try
	show_editing (get_data ())
  with
	_ -> message ("Ooops...I broke. Check your cut/paste, try another web browser, then report the bug to laheadle@gmail.com")
let () = main ()
