(* Number Names
   Show how to spell out a number in English. You should support inputs up to at least one million. *)

open Core

let digits =
  [|
    "zero";
    "one";
    "two";
    "three";
    "four";
    "five";
    "six";
    "seven";
    "eight";
    "nine";
  |]

type tens_component = Prefix of string | Choice of string array | Empty

let tens_components =
  [|
    Empty;
    Choice
      [|
        "ten";
        "eleven";
        "twelve";
        "thirteen";
        "fourteen";
        "fifteen";
        "sixteen";
        "seventeen";
        "eighteen";
        "nineteen";
      |];
    Prefix "twenty";
    Prefix "thirty";
    Prefix "fourty";
    Prefix "fifty";
    Prefix "sixty";
    Prefix "seventy";
    Prefix "eighty";
    Prefix "ninety";
  |]

let format_hundred n =
  assert (0 < n && n < 1000);
  let hundreds = n / 100 in
  Option.value_exn
    (Option.merge
       (if hundreds = 0 then None else Some (digits.(hundreds) ^ " hundred"))
       (if n % 100 = 0 then None
        else
          let tens = n % 100 / 10 in
          let ones = n % 10 in
          Some
            (match tens_components.(tens) with
            | Empty -> digits.(ones)
            | Choice components -> components.(ones)
            | Prefix prefix ->
                if ones = 0 then prefix else prefix ^ " " ^ digits.(ones)))
       ~f:(fun a b -> a ^ " and " ^ b))

let magnitudes = [| ""; " thousand"; " million"; " billion"; " trillion" |]

let rec format_positive_integer ?(magnitude = 0) n =
  assert (n >= 0);
  if n = 0 then "zero"
  else
    Option.value_exn
      (Option.merge
         (if n / 1000 = 0 then None
          else
            Some (format_positive_integer ~magnitude:(magnitude + 1) (n / 1000)))
         (if magnitude = 0 && n > 1000 && n % 1000 / 100 = 0 && n % 1000 <> 0
          then Some ("and " ^ format_hundred (n % 1000))
          else if n % 1000 = 0 then None
          else Some (format_hundred (n % 1000) ^ magnitudes.(magnitude)))
         ~f:(fun a b -> a ^ " " ^ b))

let rec format_digits digits_bytes =
  match digits_bytes with
  | [] -> Some ""
  | digit :: digits_bytes ->
      Option.(
        map2
          ( (match digit with
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
            | _ -> None)
          >>| fun digit -> " " ^ digits.(digit) )
          (format_digits digits_bytes)
          ~f:( ^ ))

let rec format_parse_float_opt n =
  Option.(
    match String.chop_prefix ~prefix:"-" n with
    | Some n -> format_parse_float_opt n >>| ( ^ ) "negative "
    | None -> (
        match String.split ~on:'.' n with
        | [ whole; "" ] | [ whole ] ->
            whole |> Int.of_string_opt >>| format_positive_integer
        | [ ""; fractional ] ->
            fractional |> String.to_list |> format_digits >>| ( ^ ) "zero point"
        | [ whole; fractional ] ->
            map2 ~f:( ^ )
              (whole |> Int.of_string_opt >>| format_positive_integer)
              (fractional |> String.to_list |> format_digits >>| ( ^ ) " point")
        | _ -> None))

let () =
  Stdlib.read_line () |> format_parse_float_opt |> Option.value_exn
  |> printf "%s\n"
