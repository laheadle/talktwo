
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

let welcome = "This is Talktwo, a dialog maker. "

let instructions = Array.of_list [
  welcome ^ "You have the first word. Draft a header and a body. You can revise them once, after your partner responds.";
  welcome ^ "Somebody started a dialog with you, and created the draft below. You can read it, and draft a short response. Your partner can revise their draft once, and then you can revise yours. So you get the last word.";
  "Make your revisions, taking into account your partner's response.";
  "Last Step: Your partner is done. Revise your response into your last word, taking into account your partner's finished word.";
  welcome ^ "Here is a dialog. You can share the url. "
]

exception Bad_input

let get_data (): state =
	if !data = None then
	  let str = (Dom_html.window##location##search |> Js.to_string) in
	  if String.length str > 0 then
 		let str = StringLabels.sub str ~pos:1 ~len:(String.length str - 1) in
		let plain = (B64.decode ~alphabet:B64.uri_safe_alphabet str) in
		try
		  data := Some (Marshal.from_string plain 0 : state)
		with
		  _ ->
			raise Bad_input
	  else
		data := Some {
		  authors = Array.make 2 "";
		  headers = Array.make 4 "";
		  bodies = Array.make 4 "";
		  curr_step = 1;
		}
	else ();
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

let show_diff from _to elt =
  let from = Js.string from in
  let _to = Js.string _to in
  let diff = Jsdiff._JsDiff##diffWords(from, _to) in

  diff##forEach(fun (part : Jsdiff.part Js.t) _ _ ->
	let added = Js.Optdef.get (part##added) (fun () -> Js._false) in
	let removed = Js.Optdef.get (part##removed) (fun () -> Js._false) in
	let clazz = match added, removed with
		(t, _) when t = Js._true -> "added"
	  | (_, t) when t = Js._true -> "removed"
	  | _, _ -> ""
	in
	let span = Dom_html.createSpan doc in
	span##className <- Js.string clazz;
	Dom.appendChild span (doc##createTextNode (part##value));
	ignore(Dom.appendChild elt span))

let show_previous_pane state =
  let previous_pane = Dom_html.getElementById "previous_pane" in
  show_elt previous_pane "block";
  let h = Dom_html.getElementById "previous_header" in
  if state.curr_step = 4 then
	show_diff (get_header state 1) (get_header state 3) h
  else
	add_text h (state.curr_step - 1 |> (get_header state));
  let body = Dom_html.getElementById "previous_body" in
  if state.curr_step = 4 then
	show_diff (get_body state 1) (get_body state 3) body
  else
	add_text body (state.curr_step - 1 |> (get_body state))

let remaining elt max =
  max - (elt##value |> Js.to_string |> String.length)

let matches ~str regex =
  Regexp.regexp regex |>
	  (fun rx -> Regexp.string_match rx str 0)
  |> is_some

let is_empty elt =
  let str = elt##value |> Js.to_string in
  String.length str = 0 || matches ~str "^\\s+$"

let not_empty elt = not (is_empty elt)

let has_remaining elt max = remaining elt max >= 0

let start_counting elt counter num =
  let handler =
	(fun _ _ ->
	  delete_contents counter;
	  add_text counter (Printf.sprintf "(Max Characters remaining: %d)" (remaining elt num));
	  Lwt.return ())
  in
  ignore(Lwt_js_events.keyups elt handler);
  ignore(Lwt_js_events.focuses elt handler);
  ignore(Lwt_js_events.changes elt handler)

let header_max = 80
let body_max = 600
let author_max = 80

let get_errors check_author =
  let header = get_input "header" in
  let body = get_textarea "body" in
  let check_author_fields () =
	match check_author with None -> true
	| Some which_author ->
	  let author_elt = get_input which_author in
	  has_remaining author_elt author_max
  in
  let not_empty_author () =
	match check_author with None -> true
	| Some which_author ->
	  let author_elt = get_input which_author in
	  not_empty author_elt
  in
  let rec find_false = function
	| [] -> None 
	| (true, _) :: tail -> find_false tail
	| (false, error_msg) :: _ -> Some error_msg
  in
  find_false
	[has_remaining header header_max,
	 "Too much header! Boring!";
	 (has_remaining body body_max,
	  "Too much body! Boring!");
	 ((check_author_fields ()),
	  "That's a long name. How about a nickname?");
	 ((not_empty header),
	  "Empty header - I need something!");
	 ((not_empty body),
	  "Empty body - I need something!");
	 (not_empty_author (),
	  "Empty author - I need something!")]


let insert_first_draft state =
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
	begin
	  let txt = (get_author state n) in
	  Printf.printf "%s %s %d" author_out txt n
	end;
	add_text author_out_elt (get_author state n)
  in
  show_author "first_author_out" 0;
  show_author "second_author_out" 1;
  show_elt (Dom_html.getElementById "final_pane") "block"


let show_first_author_in_pane () =
  show_elt (Dom_html.getElementById "first_author_in_pane") "block";
  let input = get_input "first_author_in" in
  let counter = Dom_html.getElementById "first_author_counter" in
  start_counting input counter author_max


let show_second_author_in_pane state =
  show_elt (Dom_html.getElementById "second_author_in_pane") "block";
  let input = get_input "second_author_in" in
  let counter = Dom_html.getElementById "second_author_counter" in
  start_counting input counter author_max;
  let first_author = Dom_html.getElementById "first_author" in
  add_text first_author (get_author state 0)

(** Can be called more than once, so we make sure not to increment
	curr_step more than once *)
let get_next_url =
  let next_step = ref 0 in
  fun () ->
	begin
	  let state = get_data () in
	  let make_url () =
		let header_elt = get_input "header" in
		set_header state (Js.to_string header_elt##value);
		let body_elt = get_textarea "body" in
		set_body state (Js.to_string body_elt##value);
		set_data ({ state with curr_step = !next_step });
		let str =
		  (Marshal.to_string (get_data (): state) []) |>
			  B64.encode ~alphabet:B64.uri_safe_alphabet in
		(root ^ "?" ^ str)
	  in
	  if !next_step = 0 then
		next_step := state.curr_step + 1
	  else ();
	  make_url ()
	end

let show_copybox url =
  let copybox = get_input ("copybox") in
  copybox##value <- (Js.string url);
  ignore(Lwt_js_events.clicks copybox (fun _ _ ->
	Lwt.return copybox##select ()));
  show_elt copybox "inline";
  hide_instructions ()

let update_button check_author btn_txt message_txt update_fn =
  let btn = Dom_html.getElementById "input_button" in
  Dom.appendChild btn (doc##createTextNode (btn_txt |> Js.string));
  let handle () =
	let succeeded () =
	  let set_author author n =
		let author_elt = get_input author in
		let state = get_data () in
		set_author state n (author_elt##value |> Js.to_string)
	  in
	  begin
		match check_author with
		  None -> ()
		| Some ("first_author_in" as author) ->
		  set_author author 0
		| Some ("second_author_in" as author) ->
		  set_author author 1
		| Some x -> assert false
	  end;
	  update_fn ();
	  match message_txt with
		None -> Lwt.return_unit
	  | Some message_txt ->
		Lwt.return (message message_txt)
	in
	match get_errors check_author with
	  None ->
		succeeded ()
	| Some msg -> Lwt.return (message msg)
  in
  ignore(Lwt_js_events.keyups (doc) (fun ev _ ->
	if ev##keyCode = 13 then handle ()
	else Lwt.return_unit));
  ignore(Lwt_js_events.clicks btn (fun _ _ -> handle ()))

let show_dialog_state state =
  show_instructions state;
  begin
	match state.curr_step with 1 ->
	  show_first_author_in_pane ()
	| 2 ->
	  show_second_author_in_pane state
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
	let reveal_final_result () =
	  let url = get_next_url () in
	  Dom_html.window##location##replace (Js.string url)
	in
	let make_copybox_button button_text author =
	  update_button author
		button_text
		(Some "Ok, Copy this Url and send it to your partner")
		(fun () -> show_copybox (get_next_url ()))
	in
	match state.curr_step with 1 ->
	  make_copybox_button "Create First Draft" (Some "first_author_in")
	| 2 ->
	  make_copybox_button "Create First Draft" (Some "second_author_in")
	|3 ->
	  make_copybox_button "Create Final Draft" None
	| 4 ->
	  update_button None "Complete Dialog" None reveal_final_result
	| 5 -> ();
	| _ -> raise Bad_step
  end;
  begin
	match state.curr_step with 3|4 ->
	  insert_first_draft state
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
  let debug = false in
  if debug then
	show_dialog_state (get_data ())
  else
	try
	  show_dialog_state (get_data ())
	with
	  Bad_input -> message ("Bad input. Check what you entered. If it's definitely right, then try another web browser.")
	| _ -> message ("Ooops...I broke. Report the bug to laheadle@gmail.com")

let () = main ()
