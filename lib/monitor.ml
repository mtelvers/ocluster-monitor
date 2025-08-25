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


(* Export histogram module *)
module Histogram = Histogram

(* Export parser module *)
module Parser = Parser
