let cheat = "0123456789abcdef"
let int_to_str_hex n = Char.escaped cheat.[n]
let hex_char_to_int c = int_of_string ("0x" ^ (Char.escaped c))

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

let fixed_xor la lb =
  let r = ref "" in
  List.iter2 (fun ca cb -> r := !r ^ (int_to_str_hex ((hex_char_to_int ca) lxor (hex_char_to_int cb)))) la lb;
  !r

let () = 
  let la = explode "1c0111001f010100061a024b53535009181c" in
  let lb = explode "686974207468652062756c6c277320657965" in
  let output = "746865206b696420646f6e277420706c6179" in
  Format.printf (if (fixed_xor la lb) = output then ":)\n" else ":(\n" )