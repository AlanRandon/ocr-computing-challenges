(* Caesar Cipher
   Implement a Caesar cipher, both encoding and decoding. The key is an integer from 1 to 25. This cipher rotates the letters of the alphabet (A to Z). The encoding replaces each letter with the 1st to 25th next letter in the alphabet (wrapping Z to A). So key 2 encrypts "HI" to "JK", but key 20 encrypts "HI" to "BC". *)

open Core

let code_a = Char.to_int 'a'
let code_z = Char.to_int 'z'
let code_A = Char.to_int 'A'
let code_Z = Char.to_int 'Z'

let caesar ~key str =
  Bytes.of_string str
  |> Bytes.map ~f:(fun ch ->
         match ch with
         | 'a' .. 'z' ->
             Char.of_int_exn
               (((Char.to_int ch - code_a + key) % (code_z - code_a + 1))
               + code_a)
         | 'A' .. 'Z' ->
             Char.of_int_exn
               (((Char.to_int ch - code_A + key) % (code_Z - code_A + 1))
               + code_A)
         | _ -> ch)
  |> Bytes.to_string

let command =
  Command.basic ~summary:"Encode/decode with a Caesar cipher"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map key = anon ("key" %: int)
     and path = anon (maybe ("path" %: Filename_unix.arg_type)) in
     fun () ->
       print_string
         (caesar ~key
            (match path with
            | Some path -> In_channel.read_all path
            | None -> In_channel.input_all Stdlib.stdin)))

let () = Command_unix.run command
