(* Unit Converter
   Converts various units between one another. The user enters the type of unit being entered, the type of unit they want to convert to and then the value. The program will then make the conversion. *)

open Challenge_6_lib

let usage = "convert-unit <from> <to> <value>"
let spec = []

let () =
  let args = ref [] in
  Arg.parse spec (fun arg -> args := arg :: !args) usage;
  match !args with
  | [ value; to_unit; from_unit ] -> (
      match
        (Unit.parse from_unit, Unit.parse to_unit, float_of_string_opt value)
      with
      | Ok from_unit, Ok to_unit, Some value -> (
          match Unit.convert from_unit to_unit value with
          | Ok value -> print_endline @@ string_of_float value
          | Error _ -> print_endline "Invalid converstion")
      | _ -> Arg.usage spec usage)
  | _ -> Arg.usage spec usage
