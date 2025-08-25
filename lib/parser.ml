type pool_info = { name : string; capacity : int; running : int; queue : int; utilization : float }

let parse_pool_list output =
  let lines = String.split_on_char '\n' output in
  List.filter_map
    (fun line ->
      let trimmed = String.trim line in
      (* Skip connecting message and empty lines *)
      if String.length trimmed > 0 && (not (String.starts_with ~prefix:"Connecting" trimmed)) && (not (String.contains trimmed ':')) && trimmed <> "OK" then
        Some trimmed
      else None)
    lines

let parse_pool_details pool output =
  let lines = String.split_on_char '\n' output in
  let capacity = ref 0 in
  let queue_count = ref 0 in
  let running_count = ref 0 in

  (* Parse each line with a clear state machine approach *)
  let parse_state = ref `Looking in
  let in_queue_bracket = ref false in
  let queue_is_backlog = ref false in

  List.iter
    (fun line ->
      let trimmed = String.trim line in

      (* Parse capacity line *)
      if String.length trimmed > 9 && String.sub trimmed 0 9 = "capacity:" then
        let cap_str = String.trim (String.sub trimmed 9 (String.length trimmed - 9)) in
        try capacity := int_of_string cap_str with _ -> () (* Parse queue line - look for "(backlog)" or "(ready)" *)
      else if String.length trimmed > 6 && String.sub trimmed 0 6 = "queue:" then (
        parse_state := `InQueue;
        (* Check if this is a backlog (jobs) or ready (machines) queue *)
        let is_backlog =
          String.contains trimmed '('
          &&
          try
            let paren_start = String.index trimmed '(' + 1 in
            let paren_end = String.index_from trimmed paren_start ')' in
            let state = String.sub trimmed paren_start (paren_end - paren_start) in
            String.trim state = "backlog"
          with _ -> false
        in

        (* Store whether this queue contains jobs (backlog) or machines (ready) *)
        queue_is_backlog := is_backlog;

        (* Only count items if this is a backlog queue (jobs), not ready queue (machines) *)
        if is_backlog then (
          if
            (* Check if bracket starts on this line *)
            String.contains trimmed '['
          then (
            in_queue_bracket := true;
            (* Check if bracket also closes on this line *)
            if String.contains trimmed ']' then (
              in_queue_bracket := false;
              (* Single-line queue - count items *)
              try
                let queue_start = String.index trimmed '[' in
                let queue_end = String.rindex trimmed ']' in
                let queue_content = String.sub trimmed (queue_start + 1) (queue_end - queue_start - 1) in
                let queue_content = String.trim queue_content in
                if queue_content <> "" then
                  (* Count by splitting on whitespace - each job entry is separated by whitespace *)
                  let items = String.split_on_char ' ' queue_content |> List.filter (fun s -> String.trim s <> "") in
                  queue_count := List.length items
              with _ -> ())
            else
              (* Multi-line queue starts - count items on this line after '[' *)
              try
                let queue_start = String.index trimmed '[' in
                let queue_content = String.sub trimmed (queue_start + 1) (String.length trimmed - queue_start - 1) in
                let queue_content = String.trim queue_content in
                if queue_content <> "" then queue_count := !queue_count + 1
              with _ -> ()))
        else
          (* Ready queue - don't count machines, reset any bracket tracking *)
          in_queue_bracket := false
        (* Parse lines within queue brackets - only count if it's a backlog queue *))
      else if !in_queue_bracket && !queue_is_backlog then (
        if String.contains trimmed ']' then (
          (* End of queue bracket *)
          in_queue_bracket := false;
          (* Count items on this line before ']' *)
          try
            let queue_end = String.rindex trimmed ']' in
            let queue_content = String.sub trimmed 0 queue_end in
            let queue_content = String.trim queue_content in
            if queue_content <> "" then queue_count := !queue_count + 1
          with _ -> ())
        else
          (* Middle of queue bracket - count items on this line *)
          let queue_content = String.trim trimmed in
          if queue_content <> "" then queue_count := !queue_count + 1 (* Skip lines within ready queue brackets (machines, not jobs) *))
      else if !in_queue_bracket && not !queue_is_backlog then (
        if String.contains trimmed ']' then in_queue_bracket := false (* Parse registered section - transition state *))
      else if String.length trimmed >= 11 && String.sub trimmed 0 11 = "registered:" then parse_state := `InRegistered
        (* Parse disconnected section - transition state *)
      else if String.length trimmed >= 13 && String.sub trimmed 0 13 = "disconnected:" then parse_state := `InDisconnected
        (* Parse clients section - transition state *)
      else if String.length trimmed >= 8 && String.sub trimmed 0 8 = "clients:" then parse_state := `InClients (* Parse worker lines in registered section *)
      else if !parse_state = `InRegistered && String.contains trimmed '(' && String.contains trimmed ')' then
        (* Look for "X running)" pattern at end of line using simple string operations *)
        try
          let len = String.length trimmed in
          if len > 8 && String.sub trimmed (len - 8) 8 = "running)" then
            (* Find the opening parenthesis before "running)" *)
            let rec find_paren pos = if pos < 0 then raise Not_found else if trimmed.[pos] = '(' then pos else find_paren (pos - 1) in
            let paren_pos = find_paren (len - 9) in
            let count_str = String.sub trimmed (paren_pos + 1) (len - 9 - paren_pos) in
            let count_str = String.trim count_str in
            let parts = String.split_on_char ' ' count_str in
            match parts with
            | num_str :: "running" :: _ -> running_count := !running_count + int_of_string num_str
            | [ num_str ] -> running_count := !running_count + int_of_string num_str
            | _ -> ()
        with _ -> () (* No running pattern found or parsing failed *))
    lines;

  let utilization = if !capacity > 0 then float_of_int !running_count /. float_of_int !capacity else 0.0 in
  { name = pool; capacity = !capacity; running = !running_count; queue = !queue_count; utilization }

let run_command cmd =
  let ic = Unix.open_process_in cmd in
  let r = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  r

let get_pools () =
  let output = run_command "ocluster-admin show --connect ~/admin.cap" in
  parse_pool_list output

let get_pool_info pool =
  try
    let output = run_command ("ocluster-admin show --connect ~/admin.cap " ^ pool) in
    parse_pool_details pool output
  with _ ->
    (* If parsing fails, return empty/zero values - will be picked up next time *)
    { name = pool; capacity = 0; running = 0; queue = 0; utilization = 0.0 }
