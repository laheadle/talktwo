
type state = {
  authors: string array;
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
  "Welcome to Talktwo, a dialog maker. Step 1 of 4: You have the first word. Craft a header and a body. You can revise them once, after your partner responds.";
  "Welcome to Talktwo, a dialog maker. This is step 2 of 4: Your partner's draft is below. Now draft a response.
 You can revise it once (step 4), after your partner revises (step 3).
 You get the last word.";
  "Step 3 of 4: Make your revisions, taking into account your partner's response.";
  "Step 4 of 4: Your partner is done. Revise your response into your last word, taking into account your partner's finished word.";
  "Welcome to Talktwo, a dialog maker. This is a dialog. You can share the url. "
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
		  authors = Array.make 2 "";
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

let get_author state i = state.authors.(i)

let set_author state i author = state.authors.(i) <- author

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
	let previous_pane = Dom_html.getElementById "previous_pane" in
	show_elt previous_pane "block";
	let h = Dom_html.getElementById "previous_header" in
	add_text h (state.curr_step - 1 |> (get_header state));
	let body = Dom_html.getElementById "previous_body" in
	add_text body (state.curr_step -1 |> (get_body state))

let remaining elt max =
  max - (elt##value |> Js.to_string |> String.length)

let has_remaining elt max = remaining elt max >= 0

let start_counting elt counter num =
  ignore(Lwt_js_events.keyups elt (fun _ _ ->
	delete_contents counter;
	add_text counter (Printf.sprintf "(Max Characters remaining: %d)" (remaining elt num));
	Lwt.return ()))

let header_max = 80
let body_max = 600
let author_max = 80

let within_limits check_author () =
  let header = get_input "header" in
  let body = get_textarea "body" in
  let main_result = (has_remaining header header_max) &&
	(has_remaining body body_max)
  in
  let check_author_fields () =
	match check_author with None -> true
	| Some which_author ->
	  let author_elt = get_input which_author in
	  has_remaining author_elt author_max
  in
  main_result && (check_author_fields ())

let show_first_draft state =
  let header = get_input "header" in
  let body = get_textarea "body" in
  header##value <- get_header state (state.curr_step - 2) |> Js.string;
  body##value <- get_body state (state.curr_step - 2) |> Js.string

let show_input_pane state =
	let input_pane = Dom_html.getElementById "input_pane" in
	show_elt input_pane "block";
	let header = get_input "header" in
	let body = get_textarea "body" in
	let header_counter = Dom_html.getElementById "header_counter" in
	let body_counter = Dom_html.getElementById "body_counter" in
	start_counting header header_counter header_max;
	start_counting body body_counter body_max

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
  let show_author author_out n =
	let author_out_elt = Dom_html.getElementById author_out in
	add_text author_out_elt (get_author state n)
  in
  show_author "first_author_out" 0;
  show_author "second_author_out" 1;
  show_elt (Dom_html.getElementById "final_pane") "block"


let show_first_author_pane () =
  show_elt (Dom_html.getElementById "first_author_pane") "block";
  let input = get_input "first_author_in" in
  let counter = Dom_html.getElementById "first_author_counter" in
  start_counting input counter author_max


let show_second_author_pane state =
  show_elt (Dom_html.getElementById "second_author_pane") "block";
  let input = get_input "second_author_in" in
  let counter = Dom_html.getElementById "second_author_counter" in
  start_counting input counter author_max;
  let first_author = Dom_html.getElementById "first_author" in
  add_text first_author (get_author state 0)

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

let update () =
  let url = get_next_url () in
  let copybox = get_input ("copybox") in
  copybox##value <- (Js.string url);
  ignore(Lwt_js_events.clicks copybox (fun _ _ ->
	Lwt.return copybox##select ()));
  show_elt copybox "inline";
  hide_instructions ()

let update_button check_author btn_txt message_txt update_fn =
  let btn = Dom_html.getElementById "input_button" in
  Dom.appendChild btn (doc##createTextNode (btn_txt |> Js.string));
  ignore(Lwt_js_events.clicks btn (fun _ _ ->
	let succeeded () =
	  begin
		match check_author with None -> ()
		| Some ("first_author_in" as author) ->
		  let author_elt = get_input author in
		  let state = get_data () in
		  set_author state 0 (author_elt##value |> Js.to_string)
		| Some ("second_author_in" as author) ->
		  let author_elt = get_input author in
		  let state = get_data () in
		  set_author state 1 (author_elt##value |> Js.to_string)
		| None -> ()
		| Some x -> assert false
	  end;
	  update_fn ();
	  match message_txt with
		None -> Lwt.return_unit
	  | Some message_txt ->
		Lwt.return (message message_txt)
	in
	match within_limits check_author () with
	  true ->
		succeeded ()
	| false -> Lwt.return (message "Too much information! Boring!")))

let show_editing state =
  Printf.printf "step: %d" state.curr_step;flush_all();
  show_instructions state;
  begin
	match state.curr_step with 1 ->
	  show_first_author_pane ()
	| 2 ->
	  show_second_author_pane state
	| 3|4|5->()
	| _ -> raise Bad_step
  end;
  begin
	match state.curr_step with 1 | 2 | 3 | 4 ->
	  show_input_pane state
	| 5->()
	| _ -> raise Bad_step
  end;
  begin
	match state.curr_step with 2 | 3 | 4 ->
	  show_previous_pane state;
	| 1 | 5 -> ()
	| _ -> raise Bad_step
  end;
  begin
	let show_final_url () =
	  let url = get_next_url () in
	  Dom_html.window##location##replace (Js.string url)
	in
	let make_button author =
	  update_button author
		"Done"
		(Some "Ok, Copy this Url and send it to your partner")
		update
	in
	match state.curr_step with 1 ->
	  make_button (Some "first_author_in")
	| 2 ->
	  make_button (Some "second_author_in")
	|3 ->
	  make_button None
	| 4 ->
	  update_button None "All Done" None show_final_url
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
