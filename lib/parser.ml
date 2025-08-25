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

type parse_state = {
  capacity : int;
  queue_count : int;
  running_count : int;
  state : [ `Looking | `InQueue | `InRegistered | `InDisconnected | `InClients ];
  in_queue_bracket : bool;
  queue_is_backlog : bool;
}

let initial_state = { capacity = 0; queue_count = 0; running_count = 0; state = `Looking; in_queue_bracket = false; queue_is_backlog = false }

let parse_capacity_line line =
  try
    let cap_str = String.trim (String.sub line 9 (String.length line - 9)) in
    Some (int_of_string cap_str)
  with _ -> None

let parse_queue_line line =
  let is_backlog =
    String.contains line '('
    &&
    try
      let paren_start = String.index line '(' + 1 in
      let paren_end = String.index_from line paren_start ')' in
      let state = String.sub line paren_start (paren_end - paren_start) in
      String.trim state = "backlog"
    with _ -> false
  in
  is_backlog

let count_queue_items content =
  let trimmed = String.trim content in
  if trimmed = "" then 0
  else
    let items = String.split_on_char ' ' trimmed |> List.filter (fun s -> String.trim s <> "") in
    List.length items

let parse_running_count line =
  try
    let len = String.length line in
    if len > 8 && String.sub line (len - 8) 8 = "running)" then
      let rec find_paren pos = if pos < 0 then raise Not_found else if line.[pos] = '(' then pos else find_paren (pos - 1) in
      let paren_pos = find_paren (len - 9) in
      let count_str = String.sub line (paren_pos + 1) (len - 9 - paren_pos) in
      let count_str = String.trim count_str in
      let parts = String.split_on_char ' ' count_str in
      match parts with num_str :: "running" :: _ -> Some (int_of_string num_str) | [ num_str ] -> Some (int_of_string num_str) | _ -> None
    else None
  with _ -> None

let parse_line state line =
  let trimmed = String.trim line in
  let len = String.length trimmed in

  (* Parse capacity line *)
  if len > 9 && String.sub trimmed 0 9 = "capacity:" then match parse_capacity_line trimmed with Some cap -> { state with capacity = cap } | None -> state
    (* Parse queue line *)
  else if len > 6 && String.sub trimmed 0 6 = "queue:" then
    let is_backlog = parse_queue_line trimmed in
    let new_state = { state with state = `InQueue; queue_is_backlog = is_backlog } in

    if is_backlog && String.contains trimmed '[' then
      if String.contains trimmed ']' then
        (* Single-line queue - count items *)
        try
          let queue_start = String.index trimmed '[' in
          let queue_end = String.rindex trimmed ']' in
          let queue_content = String.sub trimmed (queue_start + 1) (queue_end - queue_start - 1) in
          let count = count_queue_items queue_content in
          { new_state with queue_count = count; in_queue_bracket = false }
        with _ -> new_state
      else
        (* Multi-line queue starts *)
        try
          let queue_start = String.index trimmed '[' in
          let queue_content = String.sub trimmed (queue_start + 1) (len - queue_start - 1) in
          let additional_count = if String.trim queue_content <> "" then 1 else 0 in
          { new_state with queue_count = state.queue_count + additional_count; in_queue_bracket = true }
        with _ -> { new_state with in_queue_bracket = true }
    else
      (* Ready queue - don't count machines *)
      { new_state with in_queue_bracket = false } (* Parse lines within queue brackets *)
  else if state.in_queue_bracket && state.queue_is_backlog then
    if String.contains trimmed ']' then
      (* End of queue bracket *)
      try
        let queue_end = String.rindex trimmed ']' in
        let queue_content = String.sub trimmed 0 queue_end in
        let additional_count = if String.trim queue_content <> "" then 1 else 0 in
        { state with queue_count = state.queue_count + additional_count; in_queue_bracket = false }
      with _ -> { state with in_queue_bracket = false }
    else
      (* Middle of queue bracket *)
      let additional_count = if String.trim trimmed <> "" then 1 else 0 in
      { state with queue_count = state.queue_count + additional_count } (* Skip lines within ready queue brackets *)
  else if state.in_queue_bracket && not state.queue_is_backlog then
    if String.contains trimmed ']' then { state with in_queue_bracket = false } else state (* Parse section transitions *)
  else if len >= 11 && String.sub trimmed 0 11 = "registered:" then { state with state = `InRegistered }
  else if len >= 13 && String.sub trimmed 0 13 = "disconnected:" then { state with state = `InDisconnected }
  else if len >= 8 && String.sub trimmed 0 8 = "clients:" then { state with state = `InClients } (* Parse worker lines in registered section *)
  else if state.state = `InRegistered && String.contains trimmed '(' && String.contains trimmed ')' then
    match parse_running_count trimmed with Some count -> { state with running_count = state.running_count + count } | None -> state
  else state

let parse_pool_details pool output =
  let lines = String.split_on_char '\n' output in
  let final_state = List.fold_left parse_line initial_state lines in
  let utilization = if final_state.capacity > 0 then float_of_int final_state.running_count /. float_of_int final_state.capacity else 0.0 in
  { name = pool; capacity = final_state.capacity; running = final_state.running_count; queue = final_state.queue_count; utilization }

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
