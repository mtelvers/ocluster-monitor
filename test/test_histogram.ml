open Monitor

let test_make_braille_char_dual () =
  let open Alcotest in
  let test_case left right expected =
    let actual = Histogram.make_braille_char_dual left right in
    let expected_full = 0x2800 lor expected in
    let actual_char =
      let b = Buffer.create 1 in
      Buffer.add_utf_8_uchar b (Uchar.of_int actual);
      Buffer.contents b
    in
    let expected_char =
      let b = Buffer.create 1 in
      Buffer.add_utf_8_uchar b (Uchar.of_int expected_full);
      Buffer.contents b
    in
    Printf.printf "Dual braille Left=%.2f Right=%.2f: '%s' (expected '%s')\n" left right actual_char expected_char;
    check int (Printf.sprintf "Left=%.2f, Right=%.2f" left right) expected_full actual
  in
  test_case 0.0 0.0 0x00;
  (* empty *)
  test_case 0.0 0.25 0x80;
  (* right: 1 dot *)
  test_case 0.25 0.0 0x40;
  (* left: 1 dot *)
  test_case 0.5 0.75 (0x44 lor 0xB0);
  (* left: 2 dots, right: 3 dots *)
  test_case 1.0 1.0 0xFF (* both: 4 dots each *)

let test_generate_braille_histogram_single_row () =
  let open Alcotest in
  (* Test simple progression *)
  let simple_data = [| 0.0; 0.25; 0.5; 0.75; 1.0 |] in
  let result = Histogram.generate_braille_histogram simple_data 3 1 in
  (* Expected: char0=(0,0.25), char1=(0.5,0.75), char2=(1.0,empty) -> ⢀⣴⡇ *)
  let expected = Buffer.create 3 in
  Buffer.add_utf_8_uchar expected (Uchar.of_int (0x2800 lor 0x80));
  (* ⢀ *)
  Buffer.add_utf_8_uchar expected (Uchar.of_int (0x2800 lor 0x44 lor 0xB0));
  (* ⣴ *)
  Buffer.add_utf_8_uchar expected (Uchar.of_int (0x2800 lor 0x47));
  (* ⡇ *)
  Printf.printf "Braille histogram result: '%s'\n" result;
  Printf.printf "Expected:                 '%s'\n" (Buffer.contents expected);
  check string "simple progression" (Buffer.contents expected) result;

  (* Test all zeros *)
  let zero_data = Array.make 6 0.0 in
  let zero_result = Histogram.generate_braille_histogram zero_data 3 1 in
  let actual_zeros = Buffer.create 3 in
  Buffer.add_utf_8_uchar actual_zeros (Uchar.of_int 0x2800);
  Buffer.add_utf_8_uchar actual_zeros (Uchar.of_int 0x2800);
  Buffer.add_utf_8_uchar actual_zeros (Uchar.of_int 0x2800);
  Printf.printf "Zero histogram result: '%s'\n" zero_result;
  check string "all zeros" (Buffer.contents actual_zeros) zero_result

let test_generate_braille_histogram_multi_row () =
  let open Alcotest in
  (* Test exact levels 1,2,3,4 with height=2 (8 levels total) *)
  let level_data = [| 0.25; 0.5; 0.75; 1.0 |] in
  (* levels 1,2,3,4 out of 8 *)
  let result = Histogram.generate_braille_histogram level_data 2 2 in

  (* Should be 2 rows, all data in bottom row since levels 1-4 are in bottom half *)
  let lines = String.split_on_char '\n' result in
  Printf.printf "Multi-row histogram (2 rows):\n";
  List.iteri (fun i line -> Printf.printf "  Row %d: '%s'\n" i line) lines;
  check int "two rows" 2 (List.length lines);

  (* Top row should show partial data for the boundary value 0.5 *)
  let top_row = List.nth lines 0 in
  let expected_top = Buffer.create 2 in
  Buffer.add_utf_8_uchar expected_top (Uchar.of_int 0x2800);
  (* first char: both values < 0.5, so empty *)
  Buffer.add_utf_8_uchar expected_top (Uchar.of_int 0x28FC);
  (* second char: 0.5 shows in top row *)
  check string "top row partial" (Buffer.contents expected_top) top_row

let test_generate_queue_histogram () =
  let open Alcotest in
  (* Test queue histogram with logarithmic scaling *)
  let queue_values = [| 0; 5; 10; 15; 20 |] in
  let queue_data = Array.map queue_to_log_scale queue_values in
  let result = Histogram.generate_braille_histogram queue_data 3 1 in

  (* Test that logarithmic scaling produces reasonable results *)
  Printf.printf "Queue values: [0; 5; 10; 15; 20]\n";
  Printf.printf "Log scaled:   [%.3f; %.3f; %.3f; %.3f; %.3f]\n" queue_data.(0) queue_data.(1) queue_data.(2) queue_data.(3) queue_data.(4);
  Printf.printf "Queue histogram result: '%s'\n" result;

  (* Just check we get 3 characters and it's not empty *)
  let char_count = String.length result / 3 in
  (* Each braille char is 3 bytes in UTF-8 *)
  check int "expected 3 braille characters" 3 char_count

let () =
  let open Alcotest in
  run "Histogram"
    [
      ("braille_char_dual", [ test_case "dual columns" `Quick test_make_braille_char_dual ]);
      ("generate_histogram_single", [ test_case "single row" `Quick test_generate_braille_histogram_single_row ]);
      ("generate_histogram_multi", [ test_case "multi row" `Quick test_generate_braille_histogram_multi_row ]);
      ("generate_queue", [ test_case "queue histogram" `Quick test_generate_queue_histogram ]);
    ]
