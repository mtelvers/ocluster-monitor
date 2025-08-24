open Monitor

let usage_msg = "monitor [--once] [--test]"
let once_mode = ref false
let test_mode = ref false

let speclist =
  [ ("--once", Arg.Set once_mode, " Run once and exit (for testing)"); ("--test", Arg.Set test_mode, " Test histogram generation with known values") ]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  try if !test_mode then run_test () else if !once_mode then run_once () else main_loop () with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1
