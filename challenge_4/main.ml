(** Classification
A simple classification system asks a series of yes/no questions in order to work out what type of item is being looked at. *)

type classification_tree =
  | Question of {
      question : string;
      yes : classification_tree;
      no : classification_tree;
    }
  | Item of string
[@@deriving yojson, show]

type response = Yes | No

let rec ask_question question =
  Printf.printf "%s [Y/N] " question;
  match read_line () with
  | "Y" | "y" -> Yes
  | "N" | "n" -> No
  | invalid ->
      Printf.printf "Invalid choice: %s\n" invalid;
      ask_question question

let rec pick_item tree =
  match tree with
  | Question { question; yes; no } -> (
      match ask_question question with
      | Yes -> pick_item yes
      | No -> pick_item no)
  | Item s -> s

let usage = "classification_tree <file>"

let () =
  let args = ref [] in
  Arg.parse [] (fun arg -> args := arg :: !args) usage;
  match !args with
  | [ file ] -> (
      let tree =
        In_channel.with_open_text file In_channel.input_all
        |> Yojson.Safe.from_string |> classification_tree_of_yojson
      in
      match tree with
      | Ok tree -> print_endline (pick_item tree)
      | Error err -> Printf.printf "Error: %s\n" err)
  | _ -> Arg.usage [] usage
