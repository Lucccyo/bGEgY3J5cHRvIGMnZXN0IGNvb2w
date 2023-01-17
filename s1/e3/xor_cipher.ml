let src = [|8.50; 2.07; 4.54; 3.38; 11.16; 1.81; 2.47; 3.01; 7.54; 0.20; 1.11; 5.49; 3.01; 6.65; 7.16; 3.16; 0.20; 7.58; 5.73; 6.95; 3.63; 1.01; 1.29; 0.29; 1.79; 0.27|]
let sample = Array.map (fun f -> f /. 100.) src
(* Format.printf "%f\n" (Array.fold_left (fun acc item -> acc +. item) 0.0 src) *)

let freqs bytes =
  let freqs = Array.make (Array.length src) 0. in
  let blf = Float.of_int (Bytes.length bytes) in
  Bytes.iter (fun c ->
    let code = Char.code c in
    match code with
    (* | 32 -> () *)
    | n when (n < 91 && n > 64) || (n < 123 && n > 96) ->
      if code = 32 then ();
      let i = code - (if code <= 90 then 65 else 97) in
      freqs.(i) <- freqs.(i) +. 1.
    | _ -> ()
  ) bytes;
  let freqs = Array.map (fun f -> f /. blf) freqs in
  freqs
  (* Array.iter (fun f -> Format.printf " %f " f) score *)

let score bytes =
  let freq_array = freqs bytes in
  let s = ref 0. in
  Array.iter2 (fun freq src -> s := !s +. ((freq -. src)**2.)) freq_array src;
  sqrt !s


(* let () =
  (* The higher the score, the more certain it is that it is an English text. *)
  let byte_hello = Bytes.make 5 'l' in
  Bytes.set byte_hello 0 'h';
  Bytes.set byte_hello 1 'j';
  Bytes.set byte_hello 4 'o';
  let freq_array = freq_array byte_hello in
  Format.printf "hjllo %f\n" (score freq_array) *)







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

let pp bytes = Bytes.iter (fun c -> Format.printf "%c " c) bytes; Format.printf "\n"

let () =
  let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b373630" in
  (* I need to add 0 at the end to allow the bytes transformation *)
    (*  let input = "48656C6C6F2C206D79206E616D6520697320686563746F72" in (* Hello, my name is Hector *)
        let input = "65656565656565656565" in (* eeeeeeeeee *)
        (* the second input is not english but more correct regarding score *)*)
  let bytes = bytes_of_hex_string input in

  let min_score_bytes = ref bytes in
  for n = 0 to 255 do
    let xored = bytes_xor bytes n in
    if score xored < score !min_score_bytes then min_score_bytes := xored
  done;
  pp !min_score_bytes


