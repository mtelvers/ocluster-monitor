open Monitor

type config = { once_mode : bool; test_mode : bool }

let default_config = { once_mode = false; test_mode = false }
let usage_msg = "monitor [--once] [--test]"

let parse_args () =
  let config = ref default_config in
  let speclist =
    [
      ("--once", Arg.Unit (fun () -> config := { !config with once_mode = true }), " Run once and exit (for testing)");
      ("--test", Arg.Unit (fun () -> config := { !config with test_mode = true }), " Test histogram generation with known values");
    ]
  in
  Arg.parse speclist (fun _ -> ()) usage_msg;
  !config

let () =
  let config = parse_args () in
  try if config.test_mode then run_test () else if config.once_mode then run_once () else main_loop () with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1
