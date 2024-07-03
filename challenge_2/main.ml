(** Speed Tracker
Create a program that takes a time for a car going past a speed camera, the time going past the next one and the distance between them to calculate the average speed for the car in mph. The cameras are one mile apart.
Extensions: 
1. Speed cameras know the timings of each car going past, through number plate recognition. Valid number plates are two letters, two numbers and three letters afterwards, for example XX77 787. Produce a part of the program that checks whether a number plate matches the given pattern. Tell the user either way.
2. Create a program for creating a file of details for vehicles exceeding the speed limit set for a section of road. You will need to create a suitable file with test data, including randomised number plates and times. You will then use the code you've already written to process this list to determine who is breaking the speed limit (70mph) and who has invalid number plates. *)

let seconds_per_hour = 60. *. 60.
let speed seconds = seconds /. seconds_per_hour

let is_numberplate str =
  Str.string_match
    (Str.regexp {|^[A-Za-z][A-Za-z][0-9][0-9] [A-Za-z][A-Za-z][A-Za-z]$|})
    str 0

type entry = { numberplate : string; speed : float }

let to_entries data =
  List.filter_map
    (fun line ->
      match String.split_on_char ',' line with
      | [ numberplate; seconds ] -> (
          match float_of_string_opt seconds with
          | Some seconds -> Some { numberplate; speed = speed seconds }
          | None -> None)
      | _ -> None)
    (String.split_on_char '\n' data)

let usage = "speed-tracker <file>"

let () =
  let args = ref [] in
  Arg.parse [] (fun arg -> args := arg :: !args) usage;
  match !args with
  | [ file ] ->
      let data = In_channel.with_open_text file In_channel.input_all in
      List.iter
        (fun entry ->
          if not (is_numberplate entry.numberplate) then
            print_endline "Invalid numberplate:";
          if entry.speed > 70. then print_endline "Breaking speed limit:";
          Printf.printf "%s,%f\n" entry.numberplate entry.speed)
        (to_entries data)
  | _ -> Arg.usage [] usage
