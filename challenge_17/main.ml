(* Number Table
   Write a program that takes a symbol (+,-,* or /) and a natural number (>0) and makes a table. *)

open Core
open List

let print_table table =
  let widths =
    reduce_exn
      (table >>| map ~f:String.length)
      ~f:(fun a b -> zip_exn a b >>| Tuple2.uncurry max >>| max 3)
  in
  let horizontal =
    widths
    >>| List.init ~f:(fun _ -> "━")
    >>| String.concat |> String.concat ~sep:"╋"
  in
  table
  >>| (fun row ->
        zip_exn widths row
        >>| Tuple2.uncurry (sprintf "%-*s")
        |> String.concat ~sep:"┃")
  |> String.concat ~sep:("\n" ^ horizontal ^ "\n")
  |> print_endline

let print_table (op, op_sym) n =
  let n = n + 1 in
  let op x y = op (Float.of_int x) (Float.of_int y) in
  let header = op_sym :: init n ~f:Int.to_string in
  let rows =
    init n ~f:(fun i -> Int.to_string i :: (init n ~f:(op i) >>| sprintf "%g"))
  in
  print_table (header :: rows)

module Operation = struct
  type nonrec t = Add | Sub | Mul | Div [@@deriving enumerate, sexp_of]

  let to_string = function Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

  let op = function
    | Add -> ( +. )
    | Sub -> ( -. )
    | Mul -> ( *. )
    | Div -> ( /. )
end

let command =
  Command.basic ~summary:"Draw a table of an arithmetic operation"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map op = anon ("operation" %: Arg_type.enumerated (module Operation))
     and n = anon ("n" %: int) in
     fun () -> print_table (Operation.op op, Operation.to_string op) n)

let () = Command_unix.run command
