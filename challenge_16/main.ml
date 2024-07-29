(* Kaprekar
   Determine whether a number is a Kaprekar number or not. *)

open Core

let log10floor n = Int.of_float @@ Float.log10 @@ Float.of_int n
let digits n = log10floor n + 1

let is_kaprekar n =
  let power = Int.pow 10 (digits n) in
  let sq = n * n in
  let left_digits = sq % power in
  let right_digits = sq / power in
  left_digits + right_digits = n

let () =
  for i = 1 to 1_000_000 do
    if is_kaprekar i then printf "%i\n" i
  done
