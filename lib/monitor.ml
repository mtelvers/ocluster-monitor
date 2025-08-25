open Mosaic
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

(* Mosaic styling and colors *)
let style_for_utilization util =
  if util > 0.75 then Style.(fg Red ++ bold)
  else if util > 0.50 then Style.(fg Yellow ++ bold)
  else if util > 0.25 then Style.(fg Green ++ bold)
  else Style.(fg White ++ bold)

let style_for_queue queue =
  if queue = 0 then Style.(fg White ++ bold)
  else if queue < 100 then Style.(fg Green ++ bold)
  else if queue < 250 then Style.(fg Yellow ++ bold)
  else Style.(fg Red ++ bold)

(* Mosaic UI creation functions *)
let create_header cols =
  let hist_width = max 10 ((cols - 72) / 2) in

  (* Build header to exactly match the data row layout *)
  let pool_data_part = Printf.sprintf "%-20s %8s %7s " "Pool" "Capacity" "Running" in
  let util_part = Printf.sprintf "Util History%s" (String.make (max 0 (hist_width - 12)) ' ') in
  let spacing_part = " " in
  let queue_part = Printf.sprintf "%5s " "Queue" in
  let queue_hist_part = Printf.sprintf "Queue History%s" (String.make (max 0 (hist_width - 13)) ' ') in

  let header_line = pool_data_part ^ util_part ^ spacing_part ^ queue_part ^ queue_hist_part in
  let separator = String.make (String.length header_line) '-' in

  Ui.vbox [ Ui.text header_line; Ui.text separator ]

let create_pool_line pool_info util_hist queue_hist hist_width hist_height =
  let util_histogram = Histogram.generate_braille_histogram util_hist hist_width hist_height in
  let queue_histogram = Histogram.generate_braille_histogram queue_hist hist_width hist_height in

  let util_style = style_for_utilization pool_info.utilization in
  let queue_style = style_for_queue pool_info.queue in

  let pool_data = Printf.sprintf "%-20s %8d %7d " pool_info.name pool_info.capacity pool_info.running in

  if hist_height = 1 then
    (* Single line layout - horizontal composition *)
    Ui.hbox
      [
        Ui.text pool_data;
        Ui.text ~style:util_style util_histogram;
        Ui.text (Printf.sprintf " %5d " pool_info.queue);
        Ui.text ~style:queue_style queue_histogram;
      ]
  else
    (* Multi-line layout - pool info on same line as first histogram row *)
    let util_hist_lines = String.split_on_char '\n' util_histogram in
    let queue_hist_lines = String.split_on_char '\n' queue_histogram in

    let pool_data_width = String.length pool_data in
    let queue_value_width = 7 in

    (* Create all rows *)
    let all_rows =
      List.mapi
        (fun i util_line ->
          let queue_hist_line = if i < List.length queue_hist_lines then List.nth queue_hist_lines i else "" in

          if i = 0 then
            (* First row: pool info + first histogram row + queue value + queue histogram *)
            Ui.hbox
              [
                Ui.text pool_data;
                Ui.text ~style:util_style util_line;
                Ui.text (Printf.sprintf " %5d " pool_info.queue);
                Ui.text ~style:queue_style queue_hist_line;
              ]
          else
            (* Subsequent rows: just util histogram with exact same padding as pool_data + queue_value *)
            Ui.hbox
              [
                Ui.text (String.make pool_data_width ' ');
                Ui.text ~style:util_style util_line;
                Ui.text (String.make queue_value_width ' ');
                Ui.text ~style:queue_style queue_hist_line;
              ])
        util_hist_lines
    in

    (* Combine all rows vertically *)
    Ui.vbox all_rows

type pool_row_data = { pool_name : string; pool_info : pool_info; history : history }

(* Mosaic component for the main application *)
let monitor_app () =
  (* Set locale for UTF-8 support *)
  Unix.putenv "LC_ALL" "en_US.UTF-8";

  (* State hooks *)
  let terminal_size, set_terminal_size, _ = use_state { Sub.width = 80; height = 24 } in
  let pools_data, set_pools_data, _ = use_state [] in

  (* Subscribe to terminal resize events with screen clear *)
  use_subscription
    (Sub.window (fun size ->
         set_terminal_size size;
         dispatch_cmd (Cmd.batch [ Cmd.clear_screen; Cmd.repaint ])));

  (* Calculate initial history size based on current terminal size *)
  let cols = terminal_size.width in
  let rows = terminal_size.height in
  let hist_width = max 10 ((cols - 72) / 2) in
  let initial_history_size = hist_width * 2 in

  (* Initialize pool data on first render *)
  use_effect
    (fun () ->
      (* Only initialize if we don't have any pools yet *)
      (if List.length pools_data = 0 then
         let pools = Parser.get_pools () in
         let initial_data =
           List.map
             (fun pool_name ->
               let pool_info = Parser.get_pool_info pool_name in
               let history = create_history initial_history_size in
               let updated_history = add_to_history history pool_info.utilization pool_info.queue in
               { pool_name; pool_info; history = updated_history })
             pools
         in
         set_pools_data initial_data);
      None)
    ~deps:(Deps.keys [ Deps.int (List.length pools_data) ]);

  (* Resize pool history when terminal size changes *)
  use_effect
    (fun () ->
      (* Only resize if we have pool data and the size has changed *)
      (if List.length pools_data > 0 then
         let required_history_size = hist_width * 2 in
         let needs_resize =
           List.exists
             (fun pool_data ->
               let current_size = Array.length pool_data.history.util_history in
               current_size <> required_history_size)
             pools_data
         in

         if needs_resize then
           let updated_data =
             List.map
               (fun pool_data ->
                 let current_size = Array.length pool_data.history.util_history in
                 if current_size <> required_history_size then
                   let resized_history = resize_history pool_data.history required_history_size in
                   { pool_data with history = resized_history }
                 else pool_data)
               pools_data
           in
           set_pools_data updated_data);
      None)
    ~deps:(Deps.keys [ Deps.int hist_width; Deps.int (List.length pools_data) ]);

  (* Timer for updating data every 60 seconds *)
  use_timer ~every:60.0 (fun () ->
      let updated_data =
        List.map
          (fun pool_data ->
            let updated_info = Parser.get_pool_info pool_data.pool_name in
            let updated_history = add_to_history pool_data.history updated_info.utilization updated_info.queue in
            { pool_data with pool_info = updated_info; history = updated_history })
          pools_data
      in
      set_pools_data updated_data);

  (* Keyboard handler for quit *)
  use_keyboard (Input.Char (Uchar.of_char 'q')) Cmd.quit;

  (* Responsive UI based on terminal size *)
  let num_pools = List.length pools_data in
  let available_rows = rows - 3 in
  (* Header (2 lines) + status (1 line) *)
  let hist_height = max 1 (available_rows / max 1 num_pools) in

  let header = create_header cols in

  let pool_rows =
    List.map
      (fun pool_data -> create_pool_line pool_data.pool_info pool_data.history.util_history pool_data.history.queue_history hist_width hist_height)
      pools_data
  in

  let status_text = Printf.sprintf "Press 'q' to quit | Data updates every 60 seconds | Terminal: %dx%d" cols rows in
  let status = Ui.text ~style:(Style.fg Blue) status_text in

  Ui.vbox ([ header; Ui.empty ] @ pool_rows @ [ Ui.empty; status ])

(* Export histogram module *)
module Histogram = Histogram

(* Export parser module *)
module Parser = Parser
