open Core

let string_table = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

let () =
  (* Read stdin as a string into a varible *)
  let input = In_channel.input_all In_channel.stdin in
  (* Trim whitespace from input *)
  let input = String.strip input in
  match input.[0] with
  | 'S' ->
    (* Decode the string *)
    let input = String.sub input ~pos:1 ~len:(String.length input - 1) in
    let input = String.to_list input in
    let rec decode acc = function
      | [] -> acc
      | c :: cs ->
        let c = string_table.[Char.to_int c - 33] in
        decode (c :: acc) cs
    in
    let output = decode [] input in
    let output = String.of_char_list (List.rev output) in
    Printf.printf "%s\n" output
  | _ -> failwith "Not supported yet"
