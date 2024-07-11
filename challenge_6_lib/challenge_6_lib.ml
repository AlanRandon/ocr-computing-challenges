module Unit = struct
  type t = {
    meters_power : int;
    seconds_power : int;
    bytes_power : int;
    grams_power : int;
    celcius_power : int;
    fahrenheit_power : int;
    multiplier : float;
  }
  [@@deriving show]

  type component = Metres | Seconds | Bytes | Grams | Celcius | Fahrenheit
  [@@deriving show]

  type parse_error =
    | MissingUnitComponent
    | SizePrefixWithoutUnit
    | UnknownUnitComponent
    | MissingPower
    | ErrorStack of parse_error * parse_error
  [@@deriving show]

  let empty =
    {
      grams_power = 0;
      celcius_power = 0;
      fahrenheit_power = 0;
      meters_power = 0;
      seconds_power = 0;
      bytes_power = 0;
      multiplier = 1.;
    }

  let approx_eq a b =
    let difference = a.multiplier -. b.multiplier in
    -.Float.epsilon < difference
    && difference < Float.epsilon
    && a.grams_power = b.grams_power
    && a.celcius_power = b.celcius_power
    && a.fahrenheit_power = b.fahrenheit_power
    && a.meters_power = b.meters_power
    && a.seconds_power = b.seconds_power
    && a.bytes_power = b.bytes_power

  type convert_error = IncompatibleUnits [@@deriving show]

  let rec simplify u value =
    if u.fahrenheit_power > 0 then
      simplify
        {
          u with
          fahrenheit_power = u.fahrenheit_power - 1;
          celcius_power = u.celcius_power + 1;
          multiplier = u.multiplier *. 5. /. 9.;
        }
        (value -. 32.)
    else (u, value)

  let convert from_unit to_unit value =
    let from_unit, value = simplify from_unit value in
    let to_unit, offset = simplify to_unit 0. in
    if { from_unit with multiplier = 1. } <> { to_unit with multiplier = 1. }
    then Error IncompatibleUnits
    else Ok ((value *. from_unit.multiplier /. to_unit.multiplier) -. offset)

  let merge u unit_type power multiplier =
    let multiplier = u.multiplier *. (multiplier ** float_of_int power) in
    match unit_type with
    | Metres -> { u with meters_power = u.meters_power + power; multiplier }
    | Seconds -> { u with seconds_power = u.seconds_power + power; multiplier }
    | Bytes -> { u with bytes_power = u.bytes_power + power; multiplier }
    | Grams -> { u with grams_power = u.grams_power + power; multiplier }
    | Celcius -> { u with celcius_power = u.celcius_power + power; multiplier }
    | Fahrenheit ->
        { u with fahrenheit_power = u.fahrenheit_power + power; multiplier }

  let chars_of_string str = str |> String.to_seq |> List.of_seq

  let result_fold f result_a result_b =
    match (result_a, result_b) with
    | Ok a, Ok b -> Ok (f a b)
    | Error err, Ok _ | Ok _, Error err -> Error err
    | Error err_a, Error err_b -> Error (ErrorStack (err_a, err_b))

  let rec parse str =
    str |> parse_components
    |> Seq.fold_left
         (fun acc components ->
           result_fold
             (fun acc components ->
               List.fold_left
                 (fun acc (ty, power, multiplier) ->
                   merge acc ty power multiplier)
                 acc components)
             acc components)
         (Ok empty)

  and parse_components str :
      ((component * int * float) list, parse_error) result Seq.t =
    str |> String.split_on_char '/' |> List.to_seq
    |> Seq.zip (Seq.ints 0 |> Seq.map (fun i -> if i mod 2 = 0 then 1 else -1))
    |> Seq.map (fun (power_multiplier, str) ->
           str |> chars_of_string |> parse_component
           |> Result.map (fun components ->
                  List.map
                    (fun (ty, power, multiplier) ->
                      (ty, power * power_multiplier, multiplier))
                    components))

  and parse_component bytes =
    match bytes with
    | [] -> Ok []
    | 'h' :: bytes ->
        Result.bind (parse_power bytes) (fun (power, bytes) ->
            Result.bind (parse_component bytes) (fun components ->
                Ok ((Seconds, power, 3600.) :: components)))
    | 'm' :: 'i' :: 'n' :: bytes ->
        Result.bind (parse_power bytes) (fun (power, bytes) ->
            Result.bind (parse_component bytes) (fun components ->
                Ok ((Seconds, power, 60.) :: components)))
    | 'c' :: bytes -> parse_component_with_size_prefix 0.01 bytes
    | 'd' :: bytes -> parse_component_with_size_prefix 0.1 bytes
    | 'k' :: bytes -> parse_component_with_size_prefix 1000. bytes
    | 'M' :: bytes -> parse_component_with_size_prefix 1000_000. bytes
    | 'G' :: bytes -> parse_component_with_size_prefix 1000_000_000. bytes
    | 'm' :: bytes -> (
        match parse_component_without_size_prefix bytes with
        | Ok ((unit_type, power, multiplier) :: components) ->
            Ok ((unit_type, power, multiplier *. 0.001) :: components)
        | _ -> parse_component_without_size_prefix ('m' :: bytes))
    | bytes -> parse_component_without_size_prefix bytes

  and parse_component_with_size_prefix size bytes =
    match parse_component_without_size_prefix bytes with
    | Ok ((unit_type, power, multiplier) :: components) ->
        Ok ((unit_type, power, multiplier *. size) :: components)
    | Ok _ -> Error SizePrefixWithoutUnit
    | Error err -> Error (ErrorStack (SizePrefixWithoutUnit, err))

  and parse_component_without_size_prefix bytes =
    match bytes with
    | 'm' :: bytes -> parse_component_with_type Metres bytes
    | 'H' :: 'z' :: bytes ->
        Result.bind (parse_power bytes) (fun (power, bytes) ->
            Result.bind (parse_component bytes) (fun components ->
                Ok ((Seconds, -power, 1.) :: components)))
    | 's' :: bytes -> parse_component_with_type Seconds bytes
    | 'B' :: bytes -> parse_component_with_type Bytes bytes
    | 'b' :: bytes ->
        Result.bind (parse_power bytes) (fun (power, bytes) ->
            Result.bind (parse_component bytes) (fun components ->
                Ok ((Bytes, power, 1. /. 8.) :: components)))
    | 'l' :: bytes ->
        Result.bind (parse_power bytes) (fun (power, bytes) ->
            Result.bind (parse_component bytes) (fun components ->
                Ok ((Metres, power * 3, 0.1) :: components)))
    | 'g' :: bytes -> parse_component_with_type Grams bytes
    (* °C | ℃ *)
    | '\194' :: '\176' :: 'C' :: bytes | '\226' :: '\132' :: '\131' :: bytes ->
        parse_component_with_type Celcius bytes
    (* °F | ℉ *)
    | '\194' :: '\176' :: 'F' :: bytes | '\226' :: '\132' :: '\137' :: bytes ->
        parse_component_with_type Fahrenheit bytes
    | _ -> Error UnknownUnitComponent

  and parse_component_with_type ty bytes =
    Result.bind (parse_power bytes) (fun (power, bytes) ->
        Result.bind (parse_component bytes) (fun components ->
            Ok ((ty, power, 1.) :: components)))

  and parse_power bytes =
    match bytes with
    | '^' :: '-' :: bytes -> (
        match parse_number bytes with
        | Some power, bytes -> Ok (-power, bytes)
        | None, _ -> Error MissingPower)
    | '^' :: bytes -> (
        match parse_number bytes with
        | Some power, bytes -> Ok (power, bytes)
        | None, _ -> Error MissingPower)
    | _ -> Ok (1, bytes)

  and parse_number bytes =
    match bytes with
    | byte :: bytes -> (
        match parse_digit byte with
        | Some digit -> (
            match parse_number bytes with
            | None, bytes -> (Some digit, bytes)
            | Some n, bytes -> (Some ((10 * digit) + n), bytes))
        | None -> (None, bytes))
    | _ -> (None, bytes)

  and parse_digit digit =
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
end

let test_unit_equal parsed expected =
  match (parsed, expected) with
  | Ok parsed, Ok expected ->
      if not (Unit.approx_eq parsed expected) then (
        Printf.printf "Parsed: %s\n" (Unit.show parsed);
        Printf.printf "Expected: %s\n" (Unit.show expected);
        false)
      else true
  | Error err, _ | _, Error err ->
      Printf.printf "Parse error: %s\n" (Unit.show_parse_error err);
      false

let%test "parse km" =
  test_unit_equal ("km" |> Unit.parse)
    (Ok { Unit.empty with meters_power = 1; multiplier = 1000. })

let%test "parse km^2" =
  test_unit_equal ("km^2" |> Unit.parse)
    (Ok { Unit.empty with meters_power = 2; multiplier = 1000_000. })

let%test "parse km/h" =
  test_unit_equal ("km/h" |> Unit.parse)
    (Ok
       {
         Unit.empty with
         seconds_power = -1;
         meters_power = 1;
         multiplier = 1000. /. 3600.;
       })

let%test "parse m/s^2" =
  test_unit_equal ("m/s^2" |> Unit.parse)
    (Ok
       { Unit.empty with seconds_power = -2; meters_power = 1; multiplier = 1. })

let%test "parse kB" =
  test_unit_equal ("kB" |> Unit.parse)
    (Ok { Unit.empty with bytes_power = 1; multiplier = 1000. })

let%test "parse l = dm^3" =
  test_unit_equal ("l" |> Unit.parse) ("dm^3" |> Unit.parse)

let%test "parse l" =
  test_unit_equal ("l" |> Unit.parse)
    (Ok { Unit.empty with meters_power = 3; multiplier = 0.001 })
