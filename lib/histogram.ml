(** Braille histogram generation module *)

(** Braille dot positions: Left column: 0x01 (top), 0x02, 0x04, 0x40 (bottom) Right column: 0x08 (top), 0x10, 0x20, 0x80 (bottom) *)
let braille_base = 0x2800

let left_bits = [ 0x40; 0x04; 0x02; 0x01 ]
let right_bits = [ 0x80; 0x20; 0x10; 0x08 ]

(** Create dots for a single column based on height (0.0 to 1.0) *)
let make_braille_char_column bits height =
  (* Convert height (0.0 to 1.0) to number of dots (0 to 4) *)
  let level = int_of_float (height *. 4.0) in
  List.fold_left (fun (dots, l) mask -> ((if level >= l then dots lor mask else dots), l + 1)) (0, 1) bits |> fst

(** Create a braille character with dual columns *)
let make_braille_char_dual left_height right_height =
  let left_dots = make_braille_char_column left_bits left_height in
  let right_dots = make_braille_char_column right_bits right_height in
  braille_base lor left_dots lor right_dots

(** Create a braille character with single column (left only) *)
let make_braille_char height = make_braille_char_dual height 0.0

(** Generate a braille histogram from float data *)
let generate_braille_histogram data width height =
  let len = Array.length data in
  if len = 0 then String.make width ' '
  else
    let result = Buffer.create ((width * height) + height - 1) in
    let data_pairs =
      List.init width (fun i ->
          let left_idx = i * 2 in
          let right_idx = left_idx + 1 in
          let left = if left_idx < len then data.(left_idx) else 0.0 in
          let right = if right_idx < len then data.(right_idx) else 0.0 in
          (left, right))
    in

    (* Generate histogram row by row, from top to bottom *)
    for row = 0 to height - 1 do
      if row > 0 then Buffer.add_char result '\n';

      (* Each braille character represents 2 data points *)
      List.iter
        (fun (left_normalized, right_normalized) ->
          (* Calculate which portion of the full height each column should show *)
          (* For this row, calculate which slice of the total height to show *)
          (* Each row represents 1/height of the total range *)
          let row_height_fraction = 1.0 /. float_of_int height in
          let row_bottom_level = float_of_int (height - row - 1) *. row_height_fraction in
          let row_top_level = float_of_int (height - row) *. row_height_fraction in

          let height_in_row n = if n <= row_bottom_level then 0.0 else if n >= row_top_level then 1.0 else (n -. row_bottom_level) /. row_height_fraction in

          let braille_char = make_braille_char_dual (height_in_row left_normalized) (height_in_row right_normalized) in
          Buffer.add_utf_8_uchar result (Uchar.of_int braille_char))
        data_pairs
    done;

    Buffer.contents result

(** Generate ASCII histogram for ncurses compatibility *)
let generate_ascii_histogram data width =
  let len = Array.length data in
  if len = 0 then String.make width ' '
  else
    let max_val = Array.fold_left max 0.0 data in
    let result = Buffer.create width in

    for i = 0 to width - 1 do
      let data_idx = i * 2 in
      let value = if data_idx < len then data.(data_idx) else 0.0 in
      let normalized = if max_val > 0.0 then value /. max_val else 0.0 in

      let char =
        if normalized = 0.0 then ' ' else if normalized < 0.25 then '.' else if normalized < 0.5 then ':' else if normalized < 0.75 then 'o' else '#'
      in
      Buffer.add_char result char
    done;

    Buffer.contents result
