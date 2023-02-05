let pp bytes = Bytes.iter (fun c -> Format.printf "%c " (if Char.code c > 31 && Char.code c < 127 then c else '?')) bytes; Format.printf "\n"

let int_to_hex_char n =
  if n > 15 || n < 0 then assert false;
  if n < 10 then Char.chr (48 + n)
  else Char.chr (65 + n - 10)

let chars_glue tab_c = String.init 2 (fun i -> tab_c.(i))

let int_to_2_digit_hex_char n =
  if n > 255 then assert false;
  let d0 = n / 16 in
  if d0 = 0 then chars_glue [|'0';(int_to_hex_char n)|]
  else let d1 = n mod 16 in chars_glue [|(int_to_hex_char d0);(int_to_hex_char d1)|]

let () =
  let key = [|'I';'C';'E'|] in
  let fd = open_in "s1/e5/data" in
  let plaintext_list = ref [] in
  try
    while true do
      plaintext_list := !plaintext_list @ [input_line fd];
    done
  with End_of_file -> close_in fd;
  let plaintext = String.concat " " !plaintext_list in
  let bytes = Bytes.of_string plaintext in
  Bytes.iteri (fun i c ->
        let n = Char.code c lxor Char.code key.(i mod 3) in
        Format.printf "%s " (int_to_2_digit_hex_char n)) bytes;
  Format.printf "\n"
