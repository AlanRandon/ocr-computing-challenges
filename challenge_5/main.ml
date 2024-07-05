(* Fruit Machine
   Write a program to simulate a Fruit Machine that displays three symbols at random from Cherry, Bell, Lemon, Orange, Star, Skull.
   The player starts with £1 credit, with each go costing 20p. If the Fruit Machine "rolls" two of the same symbol, the user wins 50p. The player wins £1 for three of the same and £5 for 3 Bells. The player loses £1 if two skulls are rolled and all of his/her money if three skulls are rolled. The player can choose to quit with the winnings after each roll or keep playing until there is no money left. *)

type symbol = Cherry | Bell | Lemon | Orange | Star | Skull

let choice arr = arr.(Random.int (Array.length arr))
let symbols = [| Cherry; Bell; Lemon; Orange; Star; Skull |]

let show symbol =
  match symbol with
  | Cherry -> "🍒"
  | Bell -> "🔔"
  | Lemon -> "🍋"
  | Orange -> "🍊"
  | Star -> "🌟"
  | Skull -> "💀"

let credit_opt pennies = if pennies < 0 then None else Some pennies

let roll pennies =
  Option.bind
    (pennies - 20 |> credit_opt)
    (fun pennies ->
      let symbol_1 = choice symbols in
      let symbol_2 = choice symbols in
      let symbol_3 = choice symbols in
      Printf.printf "%s %s %s\n" (show symbol_1) (show symbol_2) (show symbol_3);
      (match (symbol_1, symbol_2, symbol_3) with
      | Skull, Skull, Skull -> 0
      | Skull, Skull, _ | _, Skull, Skull | Skull, _, Skull -> pennies - 100
      | Bell, Bell, Bell -> pennies + 500
      | a, b, c -> (
          match (a = b, b = c) with
          | true, true -> pennies + 100
          | true, false | false, true -> pennies + 50
          | false, false -> pennies))
      |> credit_opt)

let rec ask_continue () =
  Printf.printf "Continue? [Y/N] ";
  match read_line () with
  | "Y" | "y" -> true
  | "N" | "n" -> false
  | invalid ->
      Printf.printf "Invalid choice: %s\n" invalid;
      ask_continue ()

let rec game pennies =
  match pennies with
  | Some pennies ->
      Printf.printf "Credit: %dp\n" pennies;
      if ask_continue () then game (roll pennies)
      else Printf.printf "Finished with %dp\n" pennies
  | None -> print_endline "Out of credit"

let () =
  Random.self_init ();
  game (Some 100)
