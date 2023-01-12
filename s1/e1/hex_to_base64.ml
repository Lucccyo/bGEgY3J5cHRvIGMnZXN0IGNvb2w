let b64_to_char = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let int_of_hex_char c =
  let c = Char.code c in
  let d = c - Char.code '0' in
  if 0 <= d && d < 10 then d else
  let d = c - Char.code 'A' in
  if 0 <= d && d < 6 then d + 10 else
  let d = c - Char.code 'a' in
  if 0 <= d && d < 6 then d + 10 else
  assert false

let bytes_of_hex_string hex_str =
  let l = String.length hex_str in
  if l mod 2 == 1 then assert false;
  Bytes.init (l/2) (fun i -> Char.chr (
    (int_of_hex_char hex_str.[2*i] lsl 4) lor (int_of_hex_char hex_str.[2*i+1])))


let b64_of_bytes bytes =
  let blen = Bytes.length bytes in
  let b64len = 4 * (blen / 3) + (match blen mod 3 with 0 -> 0 | 1 -> 2 | _ -> 3) in
  let bget i = if i < blen then Char.code (Bytes.get bytes i) else 0 in
  let b64s = String.init b64len (fun i ->
    let deb = 3 * (i / 4) in
    let b64 = match i mod 4 with
      | 0 -> bget deb lsr 2
      | 1 -> ((bget deb land 3) lsl 4) lor (bget (deb + 1) lsr 4)
      | 2 -> ((bget (deb + 1) land 15) lsl 2) lor (bget (deb + 2) lsr 6)
      | _ -> (bget (deb + 2)) land 63 in
    b64_to_char.[b64]) in
    b64s

let () =
  let input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" in
  let bytes = bytes_of_hex_string input in
  Format.printf "%s\n" (b64_of_bytes bytes)