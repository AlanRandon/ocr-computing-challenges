(* Years in a Range
   Write a program to count the number years in a range that has a repeated digit. For example, 2012 has a repeated digit, but 2013 does not. *)

open Core
module IntSet = Set.Make (Int)

let rec digits ?(magnitude = 1) n =
  let open Sequence in
  let x = n / magnitude in
  if x = 0 then empty
  else append (digits ~magnitude:(magnitude * 10) n) (singleton (x % 10))

let has_repeats seq =
  let elements = ref IntSet.empty in
  seq
  |> Sequence.find ~f:(fun element ->
         let found = Set.mem !elements element in
         elements := Set.add !elements element;
         found)
  |> Option.is_some

let has_repeated_digits n = digits n |> has_repeats

let command =
  Command.basic
    ~summary:"Count the number of years in a range with a repeated digit"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map start = anon ("start" %: int) and stop = anon ("stop" %: int) in
     fun () ->
       let open Sequence in
       range ~stop:`inclusive start stop
       >>| has_repeated_digits |> count ~f:Fun.id |> printf "%i\n")

let () = Command_unix.run command
