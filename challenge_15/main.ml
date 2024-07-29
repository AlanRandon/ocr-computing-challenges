(* Pangrams
   "The quick brown fox jumps over the lazy dog"; note how all 26 English-language letters are used in the sentence. Your goal is to implement a program that takes a series of strings (one per line) and prints either True (the given string is a pangram), or False if it is not. *)

open Core
open Sequence

let letters =
  range ~stop:`inclusive (Char.to_int 'a') (Char.to_int 'z') >>| Char.of_int_exn

let () =
  try
    repeat () >>| Stdlib.read_line >>| String.lowercase
    >>| (fun line -> for_all ~f:(String.contains line) letters)
    >>| (fun is_pangram -> if is_pangram then "True" else "False")
    |> iter ~f:print_endline
  with End_of_file -> ()
