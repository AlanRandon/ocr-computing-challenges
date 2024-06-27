(** Factorial Finder 
The Factorial of a positive integer, n, is defined as the product of the sequence n, n-1, n-2, ...1 and the factorial of zero, 0, is defined as being 1. Solve this using both loops and recursion. *)

let rec factorial_rec n = if n <= 1 then n else n * factorial_rec (n - 1)

let factorial_loop n =
  let x = ref 1 in
  for i = 1 to n do
    x := !x * i
  done;
  !x

let usage = "factorial <rec|loop> <number>"
let spec = []

let () =
  let args = ref [] in
  Arg.parse spec (fun arg -> args := arg :: !args) usage;
  match !args with
  | [ n; "rec" ] -> (
      match int_of_string_opt n with
      | Some n -> Printf.printf "%i\n" (factorial_rec n)
      | None -> Arg.usage spec usage)
  | [ n; "loop" ] -> (
      match int_of_string_opt n with
      | Some n -> Printf.printf "%i\n" (factorial_loop n)
      | None -> Arg.usage spec usage)
  | _ -> Arg.usage spec usage
