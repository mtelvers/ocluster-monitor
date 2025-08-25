open Notty
open Nottui

module Lwd = struct
  include Lwd

  module Infix = struct
    let ( let$ ) x f = Lwd.map x ~f
    let ( let$* ) x f = Lwd.bind x ~f
  end
end

open Parser

type history = { util_history : float array; queue_history : float array }

let create_history size = { util_history = Array.make size 0.0; queue_history = Array.make size 0.0 }

(* Convert queue length to logarithmic scale for histogram display *)
let queue_to_log_scale queue = if queue = 0 then 0.0 else log10 (float_of_int queue) /. 4.0

let add_to_history history util queue =
  let size = Array.length history.util_history in
  (* Shift all elements right by 1 position *)
  if size > 1 then (
    Array.blit history.util_history 0 history.util_history 1 (size - 1);
    Array.blit history.queue_history 0 history.queue_history 1 (size - 1));
  (* Insert new data at position 0 (most recent) *)
  if size > 0 then (
    history.util_history.(0) <- util;
    history.queue_history.(0) <- queue_to_log_scale queue);
  history

let resize_history history new_size =
  let old_size = Array.length history.util_history in
  if new_size = old_size then history
  else
    let new_util = Array.make new_size 0.0 in
    let new_queue = Array.make new_size 0.0 in
    let copy_count = min old_size new_size in
    if copy_count > 0 then (
      Array.blit history.util_history 0 new_util 0 copy_count;
      Array.blit history.queue_history 0 new_queue 0 copy_count);
    { util_history = new_util; queue_history = new_queue }

(* Notty styling and colors *)
let attr_for_utilization util =
  if util > 0.75 then A.(fg red ++ st bold)
  else if util > 0.50 then A.(fg yellow ++ st bold)
  else if util > 0.25 then A.(fg green ++ st bold)
  else A.(fg white ++ st bold)

let attr_for_queue queue =
  if queue = 0 then A.(fg white ++ st bold)
  else if queue < 100 then A.(fg green ++ st bold)
  else if queue < 250 then A.(fg yellow ++ st bold)
  else A.(fg red ++ st bold)

(* Notty image creation functions *)
let create_header_image width =
  let hist_width = max 10 ((width - 72) / 2) in

  (* Build header to exactly match the data row layout *)
  (* Pool data format: "%-20s %8d %7d " = 20+1+8+1+7+1 = 38 chars *)
  let pool_data_part = Printf.sprintf "%-20s %8s %7s " "Pool" "Capacity" "Running" in
  let util_part = Printf.sprintf "Util History%s" (String.make (max 0 (hist_width - 12)) ' ') in
  (* "Util History" = 12 chars *)
  let spacing_part = " " in
  (* 1 space + 5 chars for queue = 6 total *)
  let queue_part = Printf.sprintf "%5s " "Queue" in
  (* 5 chars for queue + 1 space *)
  let queue_hist_part = Printf.sprintf "Queue History%s" (String.make (max 0 (hist_width - 13)) ' ') in
  (* "Queue History" = 13 chars *)

  let header_line = pool_data_part ^ util_part ^ spacing_part ^ queue_part ^ queue_hist_part in
  let actual_header_width = String.length header_line in
  let separator = String.make actual_header_width '-' in
  I.(string A.empty header_line <-> string A.empty separator)

let create_pool_line_image pool_info util_hist queue_hist hist_width hist_height =
  let util_histogram = Histogram.generate_braille_histogram util_hist hist_width hist_height in
  let queue_histogram = Histogram.generate_braille_histogram queue_hist hist_width hist_height in

  let util_attr = attr_for_utilization pool_info.utilization in
  let queue_attr = attr_for_queue pool_info.queue in

  let pool_data = Printf.sprintf "%-20s %8d %7d " pool_info.name pool_info.capacity pool_info.running in

  if hist_height = 1 then
    (* Single line layout - horizontal composition *)
    let pool_info_img = I.string A.empty pool_data in
    let util_hist_img = I.string util_attr util_histogram in
    let queue_value_img = I.string A.empty (Printf.sprintf " %5d " pool_info.queue) in
    let queue_hist_img = I.string queue_attr queue_histogram in
    I.(pool_info_img <|> util_hist_img <|> queue_value_img <|> queue_hist_img)
  else
    (* Multi-line layout - pool info on same line as first histogram row *)
    let util_hist_lines = String.split_on_char '\n' util_histogram in
    let queue_hist_lines = String.split_on_char '\n' queue_histogram in

    (* Calculate exact padding needed to match pool_data width *)
    let pool_data_width = String.length pool_data in
    let queue_value_width = 7 in
    (* " %5d " format *)

    (* Create all rows *)
    let all_rows =
      List.mapi
        (fun i util_line ->
          let util_img = I.string util_attr util_line in
          let queue_hist_line = if i < List.length queue_hist_lines then List.nth queue_hist_lines i else "" in
          let queue_hist_img = I.string queue_attr queue_hist_line in

          if i = 0 then
            (* First row: pool info + first histogram row + queue value + queue histogram *)
            let pool_info_img = I.string A.empty pool_data in
            let queue_value_img = I.string A.empty (Printf.sprintf " %5d " pool_info.queue) in
            I.(pool_info_img <|> util_img <|> queue_value_img <|> queue_hist_img)
          else
            (* Subsequent rows: just util histogram with exact same padding as pool_data + queue_value *)
            let left_padding = I.string A.empty (String.make pool_data_width ' ') in
            let queue_value_spacing = I.string A.empty (String.make queue_value_width ' ') in
            I.(left_padding <|> util_img <|> queue_value_spacing <|> queue_hist_img))
        util_hist_lines
    in

    (* Combine all rows vertically *)
    match all_rows with
    | [] -> I.empty
    | first :: rest -> List.fold_left I.( <-> ) first rest

type pool_row_data = { pool_name : string; pool_info : pool_info; history : history }

let main_loop () =
  (* Set locale for UTF-8 support *)
  Unix.putenv "LC_ALL" "en_US.UTF-8";

  (* Create Lwd variables for reactive state *)
  let terminal_size = Lwd.var (80, 24) in
  let last_update = Lwd.var (Unix.time ()) in
  let quit_var = Lwd.var false in

  (* Create Lwd_table for pool data *)
  let pool_table = Lwd_table.make () in

  (* Calculate initial history size based on default terminal size *)
  let initial_cols = 80 in
  let initial_hist_width = max 10 ((initial_cols - 72) / 2) in
  let initial_history_size = initial_hist_width * 2 in
  (* 2 data points per histogram character *)

  (* Initialize pool data *)
  let pools = Parser.get_pools () in
  let num_pools = List.length pools in
  List.iter
    (fun pool_name ->
      let pool_info = Parser.get_pool_info pool_name in
      let history = create_history initial_history_size in
      (* Add the initial sample to the history *)
      let updated_history = add_to_history history pool_info.utilization pool_info.queue in
      let data = { pool_name; pool_info; history = updated_history } in
      Lwd_table.append' pool_table data)
    pools;

  (* Create reactive UI components *)
  let header_ui =
    let open Lwd.Infix in
    let$* _rows, cols = Lwd.get terminal_size in
    Lwd.pure (Ui.atom (create_header_image cols))
  in

  let pool_rows_ui =
    let open Lwd.Infix in
    let$* rows, cols = Lwd.get terminal_size in
    let hist_width = max 10 ((cols - 72) / 2) in

    (* Note: History resizing is handled in the data update loop, not here *)
    (* This avoids unsafe mutations during reactive computation *)

    (* Calculate histogram height: (total_rows - header(2) - status(1)) / num_pools *)
    let available_rows = rows - 3 in
    (* 2 for header + separator, 1 for status *)
    let hist_height = max 1 (available_rows / max 1 num_pools) in

    let pool_images =
      Lwd_table.map_reduce
        (fun _row pool_data -> create_pool_line_image pool_data.pool_info pool_data.history.util_history pool_data.history.queue_history hist_width hist_height)
        (I.empty, I.( <-> )) pool_table
    in
    let$* image = pool_images in
    Lwd.pure (Ui.atom image)
  in

  let status_ui =
    let open Lwd.Infix in
    let$* rows, cols = Lwd.get terminal_size in
    let status_text = Printf.sprintf "Press 'q' to quit | Data updates every 60 seconds | Terminal: %dx%d" cols rows in
    let status_image = I.string A.(fg blue) status_text in
    Lwd.pure (Ui.atom status_image)
  in

  (* Combine UI elements - complete version *)
  let main_ui =
    let open Lwd.Infix in
    let$* header = header_ui in
    let$* pools = pool_rows_ui in
    let$* status = status_ui in
    let combined_ui = Ui.vcat [ header; Ui.atom I.empty; pools; Ui.atom I.empty; status ] in
    let keyboard_ui =
      Ui.keyboard_area
        (function
          | `ASCII 'q', [] ->
              Lwd.set quit_var true;
              `Handled
          | _ -> `Unhandled)
        combined_ui
    in
    Lwd.pure keyboard_ui
  in

  (* Add resize sensor *)
  let full_ui =
    let open Lwd.Infix in
    let$* ui = main_ui in
    Lwd.pure
      (Ui.size_sensor
         (fun ~w ~h ->
           let current_size = Lwd.peek terminal_size in
           if current_size <> (h, w) then Lwd.set terminal_size (h, w))
         ui)
  in

  (* Update function *)
  let update_data () =
    let current_time = Unix.time () in
    let last_upd = Lwd.peek last_update in
    if current_time -. last_upd >= 60.0 then (
      Lwd.set last_update current_time;

      (* Update each pool's data *)
      let _rows, cols = Lwd.peek terminal_size in
      let hist_width = max 10 ((cols - 72) / 2) in
      let required_history_size = hist_width * 2 in

      let rec update_row row_opt =
        match row_opt with
        | None -> ()
        | Some row ->
            (match Lwd_table.get row with
            | None -> ()
            | Some pool_data ->
                let updated_info = Parser.get_pool_info pool_data.pool_name in
                (* Resize history if needed before adding new data *)
                let current_size = Array.length pool_data.history.util_history in
                let history_to_update =
                  if current_size <> required_history_size then resize_history pool_data.history required_history_size else pool_data.history
                in
                let updated_history = add_to_history history_to_update updated_info.utilization updated_info.queue in
                Lwd_table.set row { pool_data with pool_info = updated_info; history = updated_history });
            update_row (Lwd_table.next row)
      in
      update_row (Lwd_table.first pool_table))
    else
      (* Trigger a tiny update to keep UI responsive for resize events *)
      let current_size = Lwd.peek terminal_size in
      Lwd.set terminal_size current_size
  in

  (* Run the UI *)
  Printf.printf "Starting OCaml Cluster Monitor...\n%!";
  Nottui_unix.run ~quit:quit_var ~quit_on_escape:true ~quit_on_ctrl_q:true ~tick_period:2.0 ~tick:update_data full_ui

let run_once () =
  Printf.printf "OCaml Cluster Monitor - Single Run Mode\n\n";
  let pools = Parser.get_pools () in

  Printf.printf "%-18s %8s %7s %5s %s\n" "Pool" "Capacity" "Running" "Queue" "Utilization";
  Printf.printf "%s\n" (String.make 60 '-');

  List.iter
    (fun pool ->
      let info = Parser.get_pool_info pool in
      let util_percent = info.utilization *. 100.0 in
      let util_bar = String.make (int_of_float (util_percent /. 5.0)) '#' in
      Printf.printf "%-18s %8d %7d %5d %.1f%% %s\n" info.name info.capacity info.running info.queue util_percent util_bar)
    pools;

  Printf.printf "\nNote: Run without --once flag for real-time monitoring with braille histograms.\n"

let run_test () =
  Printf.printf "OCaml Cluster Monitor - Histogram Test Mode\n\n";

  (* Test braille histogram with known arithmetic progression *)
  Printf.printf "Testing braille histogram generation:\n";
  Printf.printf "Width: 20 characters, Height: 1 row\n\n";

  (* Test 1: Simple 4-step progression *)
  let simple_data = [| 0.0; 0.25; 0.5; 0.75; 1.0 |] in
  Printf.printf "Simple data: ";
  Array.iteri (fun _i v -> Printf.printf "%.2f " v) simple_data;
  Printf.printf "\n";
  let simple_hist = Histogram.generate_braille_histogram simple_data 5 1 in
  Printf.printf "0→1 in steps: %s\n" simple_hist;

  (* Test 2: Linear progression 0.0 to 1.0 *)
  let linear_data = Array.init 20 (fun i -> float_of_int i /. 19.0) in
  let linear_hist = Histogram.generate_braille_histogram linear_data 20 1 in
  Printf.printf "Linear 0→1.0:  %s\n" linear_hist;

  (* Test 2: All zeros *)
  let zero_data = Array.make 20 0.0 in
  let zero_hist = Histogram.generate_braille_histogram zero_data 20 1 in
  Printf.printf "All zeros:    %s\n" zero_hist;

  (* Test 3: All ones *)
  let ones_data = Array.make 20 1.0 in
  let ones_hist = Histogram.generate_braille_histogram ones_data 20 1 in
  Printf.printf "All ones:     %s\n" ones_hist;

  (* Test 4: Sine wave *)
  let sine_data =
    Array.init 20 (fun i ->
        let x = float_of_int i *. 2.0 *. Float.pi /. 19.0 in
        (sin x +. 1.0) /. 2.0)
  in
  let sine_hist = Histogram.generate_braille_histogram sine_data 20 1 in
  Printf.printf "Sine wave:    %s\n" sine_hist;

  Printf.printf "\nTesting queue histogram generation:\n";
  Printf.printf "Width: 20 characters\n\n";

  (* Test queue histograms using logarithmic scale *)
  let queue_linear = Array.init 20 (fun i -> queue_to_log_scale (i * 5)) in
  let queue_hist = Histogram.generate_braille_histogram queue_linear 20 1 in
  Printf.printf "Linear 0→95:  %s\n" queue_hist;

  let queue_zeros = Array.make 20 0.0 in
  let queue_zero_hist = Histogram.generate_braille_histogram queue_zeros 20 1 in
  Printf.printf "All zeros:    %s\n" queue_zero_hist;

  let queue_const = Array.init 20 (fun _ -> queue_to_log_scale 50) in
  let queue_const_hist = Histogram.generate_braille_histogram queue_const 20 1 in
  Printf.printf "All 50s:      %s\n" queue_const_hist;

  Printf.printf "\nTesting individual braille characters (4 levels):\n";
  Printf.printf "0%% height (0 dots): %s\n"
    (let b = Buffer.create 1 in
     Buffer.add_utf_8_uchar b (Uchar.of_int (Histogram.make_braille_char 0.0));
     Buffer.contents b);
  Printf.printf "25%% height (1 dot): %s\n"
    (let b = Buffer.create 1 in
     Buffer.add_utf_8_uchar b (Uchar.of_int (Histogram.make_braille_char 0.25));
     Buffer.contents b);
  Printf.printf "50%% height (2 dots): %s\n"
    (let b = Buffer.create 1 in
     Buffer.add_utf_8_uchar b (Uchar.of_int (Histogram.make_braille_char 0.50));
     Buffer.contents b);
  Printf.printf "75%% height (3 dots): %s\n"
    (let b = Buffer.create 1 in
     Buffer.add_utf_8_uchar b (Uchar.of_int (Histogram.make_braille_char 0.75));
     Buffer.contents b);
  Printf.printf "100%% height (4 dots): %s\n"
    (let b = Buffer.create 1 in
     Buffer.add_utf_8_uchar b (Uchar.of_int (Histogram.make_braille_char 1.0));
     Buffer.contents b);

  Printf.printf "\nTesting dual column braille characters:\n";
  Printf.printf "Left=0, Right=1: %s\n"
    (let b = Buffer.create 1 in
     Buffer.add_utf_8_uchar b (Uchar.of_int (Histogram.make_braille_char_dual 0.0 0.25));
     Buffer.contents b);
  Printf.printf "Left=2, Right=3: %s\n"
    (let b = Buffer.create 1 in
     Buffer.add_utf_8_uchar b (Uchar.of_int (Histogram.make_braille_char_dual 0.50 0.75));
     Buffer.contents b);
  Printf.printf "Left=1, Right=4: %s\n"
    (let b = Buffer.create 1 in
     Buffer.add_utf_8_uchar b (Uchar.of_int (Histogram.make_braille_char_dual 0.25 1.0));
     Buffer.contents b);
  Printf.printf "Left=4, Right=0: %s\n"
    (let b = Buffer.create 1 in
     Buffer.add_utf_8_uchar b (Uchar.of_int (Histogram.make_braille_char_dual 1.0 0.0));
     Buffer.contents b);

  Printf.printf "\nTesting double height histograms:\n";
  Printf.printf "Width: 10 characters, Height: 2 rows\n\n";

  (* Test with varying heights *)
  let tall_data =
    Array.init 10 (fun i ->
        let x = float_of_int i *. 2.0 *. Float.pi /. 9.0 in
        (sin x +. 1.0) /. 2.0)
  in
  let tall_hist = Histogram.generate_braille_histogram tall_data 5 2 in
  Printf.printf "Tall sine wave:\n%s\n\n" tall_hist;

  (* Test with step function *)
  let step_data = [| 0.2; 0.4; 0.6; 0.8; 1.0; 0.8; 0.6; 0.4; 0.2; 0.0 |] in
  Printf.printf "Step data values: ";
  Array.iteri (fun i v -> Printf.printf "[%d]=%.1f " i v) step_data;
  Printf.printf "\n";
  Printf.printf "Expected pairs: (0.2,0.4) (0.6,0.8) (1.0,0.8) (0.6,0.4) (0.2,0.0)\n";
  Printf.printf "Expected levels (0-8): (1.6,3.2) (4.8,6.4) (8.0,6.4) (4.8,3.2) (1.6,0.0)\n";
  let step_hist = Histogram.generate_braille_histogram step_data 5 2 in
  Printf.printf "Step function:\n%s\n\n" step_hist;

  (* Verify with exact values *)
  let exact_data = [| 0.0; 0.25; 0.5; 0.75; 1.0; 0.0 |] in
  Printf.printf "Exact test (0,0.25,0.5,0.75,1.0,0): ";
  Printf.printf "Expected pairs: (0,1) (2,3) (4,0)\n";
  let exact_hist = Histogram.generate_braille_histogram exact_data 3 2 in
  Printf.printf "Exact result:\n%s\n" exact_hist;

  (* Test showing exact levels 1,2,3,4,5,6,7,8,8,7,6,5,4,3,2,1 *)
  let level_data = [| 0.125; 0.25; 0.375; 0.5; 0.625; 0.75; 0.875; 1.0; 1.0; 0.875; 0.75; 0.625; 0.5; 0.375; 0.25; 0.125 |] in
  Printf.printf "\nExact levels test (1,2,3,4,5,6,7,8,8,7,6,5,4,3,2,1):\n";
  Printf.printf "Data values: ";
  Array.iteri (fun _i v -> Printf.printf "%.3f " v) level_data;
  Printf.printf "\n";
  Printf.printf "Expected pairs: (1,2) (3,4) (5,6) (7,8) (8,7) (6,5) (4,3) (2,1)\n";
  let level_hist = Histogram.generate_braille_histogram level_data 8 2 in
  Printf.printf "Result:\n%s\n" level_hist;

  (* Test 3 rows high (12 levels total) *)
  Printf.printf "\nTesting 3-row histogram (12 levels):\n";
  let tall_data =
    [|
      1.0 /. 12.0;
      2.0 /. 12.0;
      3.0 /. 12.0;
      4.0 /. 12.0;
      5.0 /. 12.0;
      6.0 /. 12.0;
      7.0 /. 12.0;
      8.0 /. 12.0;
      9.0 /. 12.0;
      10.0 /. 12.0;
      11.0 /. 12.0;
      12.0 /. 12.0;
      12.0 /. 12.0;
      11.0 /. 12.0;
      10.0 /. 12.0;
      9.0 /. 12.0;
      8.0 /. 12.0;
      7.0 /. 12.0;
      6.0 /. 12.0;
      5.0 /. 12.0;
      4.0 /. 12.0;
      3.0 /. 12.0;
      2.0 /. 12.0;
      1.0 /. 12.0;
    |]
  in
  Printf.printf "Data values: ";
  Array.iteri (fun i v -> Printf.printf "[%d]=%.2f " i v) tall_data;
  Printf.printf "\n";
  Printf.printf "Expected levels: 1,2,3,4,5,6,7,8,9,10,11,12,12,11,10,9,8,7,6,5,4,3,2,1\n";
  Printf.printf "Expected pairs: (1,2) (3,4) (5,6) (7,8) (9,10) (11,12) (12,11) (10,9) (8,7) (6,5) (4,3) (2,1)\n";
  let tall_hist = Histogram.generate_braille_histogram tall_data 12 3 in
  Printf.printf "3-row result:\n%s\n" tall_hist;

  (* Test first character specifically *)
  Printf.printf "\nDebugging first character (should be levels 1,2):\n";
  let first_char_data = [| 0.08; 0.17 |] in
  (* levels 1,2 out of 12 *)
  let first_char = Histogram.generate_braille_histogram first_char_data 1 3 in
  Printf.printf "Isolated (1,2): %s\n" first_char;

  (* Debug the row calculation *)
  Printf.printf "\nDebugging row calculation for 3 rows:\n";
  for row = 0 to 2 do
    let row_height_fraction = 1.0 /. 3.0 in
    let row_bottom_level = float_of_int (3 - row - 1) *. row_height_fraction in
    let row_top_level = float_of_int (3 - row) *. row_height_fraction in
    Printf.printf "Row %d: covers levels %.3f to %.3f\n" row row_bottom_level row_top_level
  done;

  Printf.printf "Data 0.08 normalized against max 0.17 = %.3f\n" (0.08 /. 0.17);
  Printf.printf "Data 0.17 normalized against max 0.17 = %.3f\n" (0.17 /. 0.17);

  Printf.printf "\nTesting ASCII histograms (for ncurses compatibility):\n";
  let ascii_linear = Histogram.generate_ascii_histogram (Array.init 20 (fun i -> float_of_int i /. 19.0)) 20 in
  Printf.printf "ASCII Linear:  %s\n" ascii_linear;

  let ascii_sine = Histogram.generate_ascii_histogram sine_data 20 in
  Printf.printf "ASCII Sine:    %s\n" ascii_sine;

  Printf.printf "\nIf histograms show patterns, generation is working!\n"

(* Export histogram module *)
module Histogram = Histogram

(* Export parser module *)
module Parser = Parser
