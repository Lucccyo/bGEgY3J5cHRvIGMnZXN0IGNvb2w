let b64  = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
let fill = "000000"

let hex_char_to_int c = int_of_string ("0x" ^ (Char.escaped c))
let char_to_int c = int_of_string ((Char.escaped c))
let (<<) = Int.shift_left
let (>>) = Int.shift_right

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

let rec to_base64 buff cl =
  match cl with
  | [] -> buff
  | c2 :: c1 :: c0:: tl -> 
    let n = ((char_to_int c2) << 4) + ((char_to_int c1) << 2) + (char_to_int c0) in
    to_base64 (buff ^ (Char.escaped b64.[n])) tl
  | _ -> assert false

let padd s =
  let bit_size = 4 * (String.length s) in
  let d = bit_size mod 6 in
  let new_s = ref "" in
  String.iter (fun c ->
    let n = hex_char_to_int c in
    new_s := !new_s ^ string_of_int (n >> 2) ^ string_of_int (n land 3))s;
  if d = 0 then !new_s else !new_s ^ String.sub fill 0 ((6 - d)/2)

let hex_to_base64 str =
  to_base64 "" (explode(padd str))

let () =
  let input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" in
  let output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" in
  Format.printf (if (hex_to_base64 input) = output then ":)\n" else ":(\n")
