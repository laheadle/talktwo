open Js


class type part =
object
  method value: js_string t readonly_prop
  method added: bool t Optdef.t readonly_prop
  method removed: bool t Optdef.t readonly_prop
end

class type jsdiff =
object
  method diffChars: js_string t -> js_string t -> part t js_array t meth
  method diffWords: js_string t -> js_string t -> part t js_array t meth
end

let _JsDiff : jsdiff t =
  Js.Unsafe.variable "JsDiff"
