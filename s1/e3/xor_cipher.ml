let src = [|8.50; 2.07; 4.54; 3.38; 11.16; 1.81; 2.47; 3.01; 7.54; 0.20; 1.11; 5.49; 3.01; 6.65; 7.16; 3.16; 0.20; 7.58; 5.73; 6.95; 3.63; 1.01; 1.29; 0.29; 1.79; 0.27|]
let sample = Array.map (fun f -> f /. 100.) src
(* Format.printf "%f\n" (Array.fold_left (fun acc item -> acc +. item) 0.0 src) *)

let freq_array bytes =
  let freqs = Array.make (Array.length src) 0. in
  let blf = Float.of_int (Bytes.length bytes) in
  Bytes.iter (fun c ->
    let code = Char.code c in
    let i = code - (if code <= 90 then 65 else 97) in
    freqs.(i) <- freqs.(i) +. 1.
  ) bytes;
  let freqs = Array.map (fun f -> f /. blf) freqs in
  freqs
  (* Array.iter (fun f -> Format.printf " %f " f) score *)

let score freq_array =
  let s = ref 0. in
  Array.iter2 (fun freq src -> s := !s +. ((freq -. src)**2.)) freq_array src;
  sqrt !s


let () =
  (* let text = "hello" in *)
  let byte_hello = Bytes.make 5 'l' in
  Bytes.set byte_hello 0 'h';
  Bytes.set byte_hello 1 'j';
  Bytes.set byte_hello 4 'o';
  let freq_array = freq_array byte_hello in
  Format.printf "hzllo %f\n" (score freq_array)







(* let int_of_hex_char c =
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
    (int_of_hex_char hex_str.[2*i] lsl 4) lor (int_of_hex_char hex_str.[2*i+1]))) *)

(* let byte_to_int bytes i = Char.code (Bytes.get bytes i) *)

(* let bytes_xor bytes_a bytes_b =
  let blen = Bytes.length bytes_a in
  if blen <> Bytes.length bytes_b then assert false;
  Bytes.init blen (fun i ->
    Char.chr ((byte_to_int bytes_a i) lxor (byte_to_int bytes_b i))) *)

(* let () =
  let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b37363" in
  let _bytes = bytes_of_hex_string input in
  Format.printf "hi\n" *)

