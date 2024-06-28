open Core

let string_table = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

let decode_token input =
  (* Trim whitespace from input *)
  let input = String.strip input in
  match input.[0] with
  | 'S' ->
    (* Decode the string *)
    let input = String.sub input ~pos:1 ~len:(String.length input - 1) in
    String.map input ~f:(fun c -> string_table.[Char.to_int c - 33])
  | _ -> failwith "Not supported yet"
  
let inverted_table =
  let table = Array.create ~len:256 '@' in
  String.iteri string_table ~f:(fun i c -> table.(Char.to_int c) <- Char.of_int_exn (i + 33));
  table

let encode_string input =
  let input = String.strip input in
  let input = String.map input ~f:(fun c -> inverted_table.(Char.to_int c)) in
  "S" ^ input
