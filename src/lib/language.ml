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