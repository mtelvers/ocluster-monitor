# ocluster-monitor

A real-time terminal monitoring tool for ocluster with Unicode braille histograms.

## Features

- Real-time pool monitoring (capacity, running jobs, queue lengths)
- Historical utilization and queue data with braille histograms
- Responsive multi-line layouts that adapt to terminal size
- Logarithmic scaling for queue visualization (1-2000+ items)

## Installation

```bash
git clone https://github.com/mtelvers/ocluster-monitor
cd ocluster-monitor
opam switch create . --deps-oonly --with-test
dune build
```

## Usage

```bash
# Real-time monitoring
ocluster-monitor

# Single snapshot
ocluster-monitor --once
```

