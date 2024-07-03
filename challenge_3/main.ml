(** Thief!
A thief has managed to find out the four digits for an online PIN code, but doesn't know the correct sequence needed to hack into the account. Design and write a program that displays all the possible combinations for any four numerical digits entered by the user. The program should avoid displaying the same combination more than once. *)

let remove arr index =
  Array.concat
    [
      Array.sub arr 0 index;
      Array.sub arr (index + 1) (Array.length arr - index - 1);
    ]

let rec permutations arr =
  match arr with
  | [| item |] -> List.to_seq [ [| item |] ]
  | _ ->
      Seq.flat_map
        (fun (index, item) ->
          Seq.map
            (fun rest -> Array.concat [ [| item |]; rest ])
            (permutations (remove arr index)))
        (Array.to_seqi arr)

let usage = "permutations <list>"

let () =
  let args = ref [] in
  Arg.parse [] (fun arg -> args := arg :: !args) usage;
  match !args with
  | [ l ] ->
      Seq.iter
        (fun arr -> print_endline (String.concat "" (Array.to_list arr)))
        (permutations (Array.of_list (String.split_on_char ',' l)))
  | _ -> Arg.usage [] usage
