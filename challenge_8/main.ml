(* Arithmetic test
   A primary school teacher wants a computer program to test the basic arithmetic skills of their students. Generate random questions (2 numbers only) consisting of addition, subtraction, multiplication and division. The system should ask the student’s name and then ask ten questions. The program should feed back if the answers are correct or not, and then generate a final score at the end.
   Extensions:
   1. Extend your program so that it stores the results somewhere. The teacher has three classes, so you need to enable the program to distinguish between them.
   2. The teacher wants to be able to log student performance in these tests. The teacher would like the program to store the last three scores for each student and to be able to output the results in alphabetical order with the student’s highest score first out of the three. *)

open Core

type question = { question : string; result : int }

let random_question () =
  match Random.int 4 with
  | 0 ->
      let lhs = Random.int 10 in
      let rhs = Random.int 10 in
      { result = lhs * rhs; question = Printf.sprintf "%d * %d = " lhs rhs }
  | 1 ->
      let lhs = Random.int 100 in
      let rhs = Random.int (100 - lhs) in
      { result = lhs + rhs; question = Printf.sprintf "%d + %d = " lhs rhs }
  | 2 ->
      let lhs = Random.int 100 in
      let rhs = Random.int lhs in
      { result = lhs - rhs; question = Printf.sprintf "%d - %d = " lhs rhs }
  | 3 ->
      let rhs = Random.int 9 + 1 in
      let lhs = rhs * Random.int 9 in
      { result = lhs / rhs; question = Printf.sprintf "%d / %d = " lhs rhs }
  | _ -> failwith "random int was unhandled"

let rec read_validate ~prompt f =
  Stdlib.print_string prompt;
  Stdlib.read_line () |> f
  |> Option.value_or_thunk ~default:(fun () ->
         Stdlib.print_endline "Invalid input";
         read_validate ~prompt f)

let run_test_and_get_score () =
  List.init 10 ~f:(fun _ ->
      let question = random_question () in
      let result = read_validate ~prompt:question.question Int.of_string_opt in
      if question.result = result then (
        Stdlib.print_endline "Correct";
        1)
      else (
        Stdlib.print_endline "Incorrect";
        0))
  |> List.fold ~init:0 ~f:( + )

module Results = Hashtbl.Make_binable (String)

let read_results ic =
  let bytes = In_channel.input_all ic in
  let len = String.length bytes in
  let buf = Bin_prot.Common.create_buf len in
  Bin_prot.Common.blit_string_buf bytes buf ~len;
  let pos_ref = ref 0 in
  Results.bin_read_t (List.bin_read_t Int.bin_read_t) buf ~pos_ref

let write_results results oc =
  let bytes =
    Bin_prot.Writer.to_bytes
      (Results.bin_writer_t (List.bin_writer_t Int.bin_writer_t))
      results
  in
  Out_channel.output_bytes oc bytes

let run_test_and_update_results results =
  let name = read_validate ~prompt:"Name: " Option.some in
  let score = run_test_and_get_score () in
  Printf.printf "Score: %d/10 \n" score;
  Hashtbl.update results name ~f:(fun scores ->
      List.take (score :: Option.value ~default:[] scores) 3)

let run_test =
  Command.basic ~summary:"Run an arithmetic test"
    Command.Param.(
      map
        (anon ("results-file" %: string))
        ~f:(fun results_file () ->
          let results =
            match Sys_unix.file_exists results_file with
            | `Yes -> (
                try
                  In_channel.with_file ~binary:true ~f:read_results results_file
                with _ ->
                  Stdlib.prerr_endline "Failed to open results file";
                  exit 1)
            | `No -> Results.create ()
            | `Unknown ->
                Stdlib.prerr_endline "Failed to open results file";
                exit 1
          in
          Random.self_init ();
          run_test_and_update_results results;
          Out_channel.with_file ~binary:true results_file
            ~f:(write_results results)))

let show_results =
  Command.basic ~summary:"Show results"
    Command.Param.(
      map
        (anon ("results-file" %: string))
        ~f:(fun results_file () ->
          printf "=== %s ===\n"
            (results_file |> Filename.basename |> Filename.chop_extension);
          let results =
            try In_channel.with_file ~binary:true ~f:read_results results_file
            with _ ->
              Stdlib.prerr_endline "Failed to open results file";
              exit 1
          in
          results |> Hashtbl.to_alist
          |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
          |> List.map ~f:(fun (name, scores) ->
                 printf "%s: %s\n" name
                   (scores |> List.map ~f:Int.to_string
                  |> String.concat ~sep:", "))
          |> ignore))

let command =
  Command.group ~summary:"Run arithmetic tests and read the results"
    [ ("test", run_test); ("show", show_results) ]

let () = Command_unix.run command
