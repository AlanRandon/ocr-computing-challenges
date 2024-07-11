(* Credit Card Validator
   Takes in a credit card number from a common credit card vendor (Visa, MasterCard, American Express, Discoverer) and validates it to make sure that it is a valid number (look into how credit cards use a checksum). *)

open Core

let digit_of_char digit =
  match digit with
  | '0' -> Some 0
  | '1' -> Some 1
  | '2' -> Some 2
  | '3' -> Some 3
  | '4' -> Some 4
  | '5' -> Some 5
  | '6' -> Some 6
  | '7' -> Some 7
  | '8' -> Some 8
  | '9' -> Some 9
  | _ -> None

let rec sum_of_digits n = if n = 0 then 0 else (n % 10) + sum_of_digits (n / 10)

let is_valid_luhn str =
  match str |> String.to_list_rev with
  | check_digit :: payload ->
      let digit =
        payload |> Sequence.of_list
        |> Sequence.map ~f:digit_of_char
        |> Sequence.mapi ~f:(fun i ->
               Option.map ~f:(fun digit ->
                   if i % 2 = 0 then digit * 2 else digit))
        |> Sequence.map ~f:(Option.map ~f:sum_of_digits)
        |> Sequence.fold ~init:(Some 0) ~f:(Option.merge ~f:( + ))
        |> Option.map ~f:(fun sum -> (10 - sum) % 10)
      in
      Option.equal ( = ) (digit_of_char check_digit) digit
  | _ -> false

let command =
  Command.basic
    ~summary:
      "Check if a number is a valid credit card number via the Luhn algorithm"
    Command.Param.(
      map
        (anon ("number" %: string))
        ~f:(fun number () ->
          if is_valid_luhn number then Stdio.print_endline "Valid"
          else Stdio.print_endline "Invalid";
          Stdlib.exit 1))

let () = Command_unix.run command
