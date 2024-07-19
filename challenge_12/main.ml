(* Quiz Maker
   Make an application which takes various questions from a file, picked randomly, and puts together a quiz for students. Each quiz can be different and then reads a key to grade the quizzes. *)

open Core

type question = {
  question : string;
  correct : string;
  incorrect : string array;
}
[@@deriving sexp, show]

type test = { questions : question array } [@@deriving sexp, show]

let toml_string_opt value =
  match value with Otoml.TomlString s -> Some s | _ -> None

let toml_list_opt value =
  match value with
  | Otoml.TomlArray lst | Otoml.TomlTableArray lst -> Some lst
  | _ -> None

let toml_find_key_opt value path ~f = Otoml.find_opt value f path |> Option.join
let toml_find_string_opt = toml_find_key_opt ~f:toml_string_opt

let toml_find_list_opt ~f value path =
  toml_find_key_opt ~f:toml_list_opt value path
  |> Option.bind ~f:(fun questions -> questions |> List.map ~f |> Option.all)

let question_of_toml_opt value =
  Option.map3
    ~f:(fun question correct incorrect -> { question; correct; incorrect })
    (toml_find_string_opt value [ "question" ])
    (toml_find_string_opt value [ "correct" ])
    (toml_find_list_opt ~f:toml_string_opt value [ "incorrect" ]
    |> Option.map ~f:Array.of_list)

let test_of_toml_opt value =
  toml_find_list_opt ~f:question_of_toml_opt value [ "questions" ]
  |> Option.map ~f:Array.of_list
  |> Option.map ~f:(fun questions -> { questions })

let knuth_shuffle_mutate a =
  let n = Array.length a in
  for i = n - 1 downto 1 do
    let k = Random.int (i + 1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done

let rec ask_question question choices =
  printf "%s\n" question;
  Array.iteri
    ~f:(fun i (_, choice) -> printf "\t%i) %s\n" (i + 1) choice)
    choices;
  print_string "> ";
  let line = Stdlib.read_line () in
  line |> int_of_string_opt
  |> Option.map ~f:(( + ) (-1))
  |> Option.bind ~f:(fun choice ->
         if choice >= 0 && choice < Array.length choices then
           Some (fst choices.(choice))
         else None)
  |> Option.value_or_thunk ~default:(fun () ->
         printf "Invalid input: %s\n" line;
         ask_question question choices)

let run_test test =
  Random.self_init ();
  knuth_shuffle_mutate test.questions;
  let score =
    Array.fold ~init:0
      ~f:(fun score question ->
        let choices =
          Array.concat
            [
              [| (true, question.correct) |];
              Array.map ~f:(fun choice -> (false, choice)) question.incorrect;
            ]
        in
        knuth_shuffle_mutate choices;
        if ask_question question.question choices then score + 1 else score)
      test.questions
  in
  printf "Score: %i/%i\n" score (Array.length test.questions)

let command =
  Command.basic ~summary:"Run test"
    Command.Param.(
      map
        (anon ("test" %: string))
        ~f:(fun test_path () ->
          let results =
            try
              In_channel.with_file ~binary:true ~f:Otoml.Parser.from_channel
                test_path
            with
            | Otoml.Parse_error (pos, message) ->
                Stdlib.prerr_endline
                  (sprintf "Failed to parse test: %s%s"
                     (Option.value_map ~default:""
                        ~f:(fun (line, col) ->
                          sprintf "%s:%i:%i" test_path line col)
                        pos)
                     message);
                exit 1
            | _ ->
                Stdlib.prerr_endline
                  (sprintf "Failed to open test: %s" test_path);
                exit 1
          in
          match test_of_toml_opt results with
          | Some test -> run_test test
          | None ->
              Stdlib.prerr_endline (sprintf "Invalid test: %s" test_path);
              exit 1))

let () = Command_unix.run command
