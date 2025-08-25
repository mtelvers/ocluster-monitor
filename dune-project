(lang dune 3.19)

(name ocluster-monitor)

(generate_opam_files true)

(source
 (github mtelvers/ocluster-monitor))

(maintainers "Mark Elvers <mark.elvers@tunbury.org>")

(license MIT)

(documentation https://github.com/mtelvers/ocluster-monitor)


(package
 (name ocluster-monitor)
 (synopsis "Real-time monitoring tool for ocluster")
 (description "A terminal-based monitoring tool for ocluster-admin with real-time histograms using Unicode braille characters. Displays pool capacity, running jobs, queue lengths, and utilization history with responsive multi-line layouts.")
 (depends ocaml dune unix str mosaic (alcotest :with-test))
 (tags
  ("monitoring" "cluster" "terminal" "ocluster" "braille" "histogram")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
