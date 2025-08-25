let () =
  Printf.printf "Starting OCaml Cluster Monitor...\n%!";
  Mosaic.run Monitor.monitor_app
