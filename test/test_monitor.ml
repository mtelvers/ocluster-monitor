open Monitor
open Parser

(* Test the business logic of the monitor module *)

let test_queue_to_log_scale () =
  let open Alcotest in
  (* Test the queue logarithmic scaling function *)
  let queue_values = [ 0; 1; 10; 100; 1000; 10000 ] in
  let log_scaled = List.map queue_to_log_scale queue_values in

  (* Test that 0 maps to 0.0 *)
  let zero_result = List.hd log_scaled in
  check (float 0.001) "zero queue maps to zero" 0.0 zero_result;

  (* Test that larger values are properly scaled between 0 and 1 *)
  let max_result = List.fold_left max 0.0 log_scaled in
  check bool "max scaled value is reasonable" true (max_result >= 0.0 && max_result <= 1.0);

  (* Test monotonic increase *)
  let pairs = List.combine queue_values log_scaled in
  let is_monotonic = List.for_all2 (fun (q1, s1) (q2, s2) -> if q1 < q2 then s1 <= s2 else true) (List.rev (List.tl (List.rev pairs))) (List.tl pairs) in
  check bool "scaling is monotonic" true is_monotonic;

  Printf.printf "Queue log scaling: %s\n" (String.concat "; " (List.map2 (Printf.sprintf "%d->%.3f") queue_values log_scaled))

let test_history_operations () =
  let open Alcotest in
  (* Test history creation and manipulation *)
  let history = create_history 5 in
  check int "history util array size" 5 (Array.length history.util_history);
  check int "history queue array size" 5 (Array.length history.queue_history);

  (* Test adding data to history *)
  let updated_history = add_to_history history 0.8 42 in
  check (float 0.001) "newest utilization" 0.8 updated_history.util_history.(0);

  (* Check that queue is log-scaled *)
  let expected_queue = queue_to_log_scale 42 in
  check (float 0.001) "newest queue log-scaled" expected_queue updated_history.queue_history.(0);

  (* Test history resize *)
  let resized_history = resize_history updated_history 3 in
  check int "resized util array size" 3 (Array.length resized_history.util_history);
  check (float 0.001) "data preserved after resize" 0.8 resized_history.util_history.(0);

  Printf.printf "History operations test completed\n"

let test_styling_functions () =
  let open Alcotest in
  (* Test that styling functions return valid styles *)
  let _high_util_style = style_for_utilization 0.9 in
  let _medium_util_style = style_for_utilization 0.6 in
  let _low_util_style = style_for_utilization 0.2 in

  (* Just verify they don't crash - these return Style.t which we can't easily test *)
  check bool "high util style created" true true;
  check bool "medium util style created" true true;
  check bool "low util style created" true true;

  let _no_queue_style = style_for_queue 0 in
  let _high_queue_style = style_for_queue 500 in

  check bool "no queue style created" true true;
  check bool "high queue style created" true true;

  Printf.printf "Styling functions test completed\n"

let test_monitor_display_creation () =
  let open Alcotest in
  (* Test monitor display creation with sample data *)
  let sample_pool_data =
    [
      {
        pool_name = "test-pool";
        pool_info = { name = "test-pool"; capacity = 10; running = 5; queue = 3; utilization = 0.5 };
        history = { util_history = [| 0.1; 0.3; 0.5; 0.7; 0.9 |]; queue_history = [| 0.0; 0.1; 0.2; 0.3; 0.4 |] };
        last_updated = Unix.time ();
      };
    ]
  in

  (* Test single-row display creation *)
  let _display_single = create_monitor_display sample_pool_data 100 1 in
  check bool "single-row display created" true true;

  (* Test multi-row display creation *)
  let _display_multi = create_monitor_display sample_pool_data 100 3 in
  check bool "multi-row display created" true true;

  Printf.printf "Monitor display creation test completed\n"

let () =
  let open Alcotest in
  run "Monitor"
    [
      ("queue_scaling", [ test_case "queue log scaling" `Quick test_queue_to_log_scale ]);
      ("history_operations", [ test_case "history operations" `Quick test_history_operations ]);
      ("styling", [ test_case "styling functions" `Quick test_styling_functions ]);
      ("display_creation", [ test_case "monitor display creation" `Quick test_monitor_display_creation ]);
    ]
