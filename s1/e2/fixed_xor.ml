let int_of_hex_char c =
  let c = Char.code c in
  let d = c - Char.code '0' in
  if 0 <= d && d < 10 then d else
  let d = c - Char.code 'A' in
  if 0 <= d && d < 6 then d + 10 else
  let d = c - Char.code 'a' in
  if 0 <= d && d < 6 then d + 10 else
  assert false

let int_to_hex_char n =
  if n >= 0 && n <= 9 then Char.(chr (n + code '0')) else
  if n >  9 && n <= 15 then Char.chr (n + 87) else
  assert false

let bytes_of_hex_string hex_str =
  let l = String.length hex_str in
  if l mod 2 == 1 then assert false;
  Bytes.init (l/2) (fun i -> Char.chr (
    (int_of_hex_char hex_str.[2*i] lsl 4) lor (int_of_hex_char hex_str.[2*i+1])))

let byte_to_int bytes i = Char.code (Bytes.get bytes i)

let bytes_xor bytes_a bytes_b =
  let blen = Bytes.length bytes_a in
  if blen <> Bytes.length bytes_b then assert false;
  Bytes.init blen (fun i ->
    Char.chr ((byte_to_int bytes_a i) lxor (byte_to_int bytes_b i)))

let bytes_to_hex_string bytes =
  let blen = Bytes.length bytes in
  String.init (blen*2) (fun i ->
    if i mod 2 = 0
    then int_to_hex_char ((Char.code (Bytes.get bytes (i/2))) lsr 4)
    else int_to_hex_char ((Char.code (Bytes.get bytes ((i-1)/2))) land 15))

let () =
  let input_a = "1c0111001f010100061a024b53535009181c" in
  let input_b = "686974207468652062756c6c277320657965" in
  let bytes_a = bytes_of_hex_string input_a in
  let bytes_b = bytes_of_hex_string input_b in
  let xored = bytes_xor bytes_a bytes_b in
  Format.printf "%s\n" (bytes_to_hex_string xored)