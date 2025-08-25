open Alcotest
open Monitor

let linux_x86_64_output =
  {|capacity: 450
queue: (ready) [toxis.caelum.ci.dev eumache clete.caelum.ci.dev
       doris.caelum.ci.dev phoebe asteria.caelum.ci.dev bremusa.ocamllabs.io
       odawa.caelum.ci.dev]
registered:
  asteria.caelum.ci.dev (0): [] (0 running)
  bremusa.ocamllabs.io (0): [] (0 running)
  clete.caelum.ci.dev (0): [] (0 running)
  doris.caelum.ci.dev (0): [] (0 running)
  eumache (0): [] (0 running)
  laodoke.caelum.ci.dev (60): [base-image-builder:opam-fedora-41@-170344s(10)
                               base-image-builder:opam-ubuntu-25.04@-170434s(10)
                               ocaml-ci:mirage/ocaml-9p-ocaml/opam:debian-12-ocaml-4.08@sha256:474656ea1593a299054f8966c700443fa0944c9534de3da94ca6dfab4a44c47a-debian-12-4.08_opam-2.4-e21ac96f25181b92542dce7b20a131ed@-67938s(10+urgent)
                               ocaml-ci:backtracking/bitv-ocaml/opam:debian-12-ocaml-4.08@sha256:474656ea1593a299054f8966c700443fa0944c9534de3da94ca6dfab4a44c47a-debian-12-4.08_opam-2.4-f7a07d2bf0f55b11fd8ad55846427c47@-81446s(10+urgent)
                               ocaml-ci:ocaml-opam/opam-compiler-ocaml/opam:debian-12-ocaml-4.08@sha256:474656ea1593a299054f8966c700443fa0944c9534de3da94ca6dfab4a44c47a-debian-12-4.08_opam-2.4-bc4a99e01d9cfa00044d4245882976d8@-275981s(10+urgent)
                               ocaml-ci:ocaml-opam/opam2web-ocaml/opam:debian-12-ocaml-4.08@sha256:474656ea1593a299054f8966c700443fa0944c9534de3da94ca6dfab4a44c47a-debian-12-4.08_opam-2.4-0d306f37eae23650bc6257bd11e04e83@-670621s(10+urgent)] (10 running)
  marpe.caelum.ci.dev (0): (inactive: admin pause) (0 running)
  odawa.caelum.ci.dev (0): [] (0 running)
  phoebe (0): [] (0 running)
  toxis.caelum.ci.dev (0): [] (0 running)
disconnected:
  iphito.caelum.ci.dev
clients: base-image-builder(1)+367m docs.ci(1) mirage-ci(1)
         mirage-deployer(1) mtelvers(10000) multicore-ci(1) ocaml-ci(900)
         ocurrent-deployer(1) opam-health-check(1)+4911991m
         opam-repo-ci(1000) oxcaml(1) sadiq(1)
Connecting to tcp:ci3.ocamllabs.io:8103... OK|}

let freebsd_x86_64_output =
  {|capacity: 20
queue: (ready) [rosemary]
registered:
  rosemary (0): [] (0 running)
disconnected:
clients: mtelvers(1) ocaml-ci(900) opam-health-check(1)+1647357m
         opam-repo-ci(1000)
Connecting to tcp:ci3.ocamllabs.io:8103... OK|}

let test_parse_pool_details_with_queue () =
  let result = Parser.parse_pool_details "linux-x86_64" linux_x86_64_output in
  check string "pool name" "linux-x86_64" result.name;
  check int "capacity" 450 result.capacity;
  check int "running jobs" 10 result.running;
  check int "queue count" 0 result.queue;
  (* No backlog queue in this example *)
  check (float 0.0001) "utilization" (10.0 /. 450.0) result.utilization

let test_parse_pool_details_no_queue () =
  let result = Parser.parse_pool_details "freebsd-x86_64" freebsd_x86_64_output in
  check string "pool name" "freebsd-x86_64" result.name;
  check int "capacity" 20 result.capacity;
  check int "running jobs" 0 result.running;
  check int "queue count" 0 result.queue;
  (* No backlog queue *)
  check (float 0.0001) "utilization" 0.0 result.utilization

let test_parse_pool_details_with_backlog_queue () =
  (* Create test data with a backlog queue *)
  let backlog_output =
    {|capacity: 100
queue: (backlog) [
       job1
       job2
       job3
]
registered:
  worker1 (10): [] (5 running)
disconnected:
clients: test(1)
Connecting to tcp:test... OK|}
  in
  let result = Parser.parse_pool_details "test-pool" backlog_output in
  check string "pool name" "test-pool" result.name;
  check int "capacity" 100 result.capacity;
  check int "running jobs" 5 result.running;
  check int "queue count" 3 result.queue;
  (* Should count 3 jobs in backlog *)
  check (float 0.0001) "utilization" 0.05 result.utilization

let test_parse_pool_list () =
  let pool_list_output = {|linux-x86_64
freebsd-x86_64
macos-arm64
Connecting to tcp:ci3.ocamllabs.io:8103... OK|} in
  let result = Parser.parse_pool_list pool_list_output in
  check (list string) "pool list" [ "linux-x86_64"; "freebsd-x86_64"; "macos-arm64" ] result

let pool_tests =
  [
    ("Parse pool details with ready queue", `Quick, test_parse_pool_details_with_queue);
    ("Parse pool details with no queue", `Quick, test_parse_pool_details_no_queue);
    ("Parse pool details with backlog queue", `Quick, test_parse_pool_details_with_backlog_queue);
    ("Parse pool list", `Quick, test_parse_pool_list);
  ]

let () = run "Parser" [ ("Pool parsing", pool_tests) ]
