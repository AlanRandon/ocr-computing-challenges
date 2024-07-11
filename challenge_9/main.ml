(* Happy Numbers
   A happy number is defined by the following process:
   Starting with any positive integer, replace the number by the sum of the squares of its digits, and repeat the process until the number equals 1 (where it will stay), or it loops endlessly in a cycle which does not include 1. Those numbers for which this process ends in 1 are happy numbers, while those that do not end in 1 are unhappy numbers. Display an example of your output here. Find the first eight happy numbers. *)

open Core

let rec sum_of_digits_squared n =
  if n = 0 then 0 else Int.pow (n % 10) 2 + sum_of_digits_squared (n / 10)

module IntSet = Set.Make (Int)

let rec is_happy ?(visited = IntSet.empty) n =
  if n = 1 then true
  else if Set.mem visited n then false
  else is_happy ~visited:(Set.add visited n) (sum_of_digits_squared n)

let () =
  for n = 1 to 10 do
    printf "%i is %s\n" n (if is_happy n then "happy" else "not happy")
  done
