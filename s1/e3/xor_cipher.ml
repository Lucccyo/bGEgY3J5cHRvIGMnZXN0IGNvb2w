let src = [|0.0651738; 0.0124248; 0.0217339; 0.0349835; 0.1041442;
            0.0197881; 0.0158610; 0.0492888; 0.0558094; 0.0009033;
            0.0050529; 0.0331490; 0.0202124; 0.0564513; 0.0596302;
            0.0137645; 0.0008606; 0.0497563; 0.0515760; 0.0729357;
            0.0225134; 0.0082903; 0.0171272; 0.0013692; 0.0145984;
            0.0007836; 0.1918182|]

let sample = Array.map (fun f -> f /. 100.) src

let freqs bytes =
  let freqs = Array.make (Array.length src) 0. in
  let blf = Float.of_int (Bytes.length bytes) in
  Bytes.iter (fun c ->
    let code = Char.code c in
    match code with
    | 32 -> freqs.(26) <- freqs.(26) +. 1.
    | n when (n < 91 && n > 64) || (n < 123 && n > 96) ->
      let i = code - (if code <= 90 then 65 else 97) in
      freqs.(i) <- freqs.(i) +. 1.
    | _ -> ()
  ) bytes;
  let freqs = Array.map (fun f -> f /. blf) freqs in
  freqs

let score bytes =
    let freq_array = freqs bytes in
    let s = ref 0. in
    Array.iter2 (fun freq src -> s := !s +. ((freq -. src)**2.)) freq_array src;
    !s

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

let byte_to_int bytes i = Char.code (Bytes.get bytes i)

let bytes_xor bytes n = Bytes.map (fun c -> Char.chr ((Char.code c) lxor n)) bytes

let pp bytes = Bytes.iter (fun c -> Format.printf "%c " (if Char.code c > 31 && Char.code c < 127 then c else '?')) bytes; Format.printf "\n"

let () =
  let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736" in
  let bytes = bytes_of_hex_string input in
  let min_score_bytes = ref bytes in
  for n = 0 to 127 do
    let xored = bytes_xor bytes n in
    if score xored < score !min_score_bytes then min_score_bytes := xored
  done;
  pp !min_score_bytes
