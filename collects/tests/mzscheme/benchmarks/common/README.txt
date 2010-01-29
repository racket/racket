To run a benchmark, assuming you have `mzscheme' in your path:
 ./auto.ss <impl-or-benchmark> ...
where <impl-or-benchmark> names an implementation as one of
   mzscheme
   bigloo
   chicken
   gambit
   larceny
   ...
or a benchmark as one of
   conform
   cpstack
   ctak
   ...
or any of the above prefixed by "no-" to skip the corresponding
<impl-or-benchmark>. To see a complete list of implementations
and benchmarks, run
 ./auto.ss --show

Naming no implementation/benchmark causes a standard set of them to be
run (as reported by --show). Similarly, if the first named
implementation/benchmak starts with "no-", the default set is used
minus the "no-"-specified implementation/benchmark.

The output is a comment line
  ; <date and time>
and then a series of lines of the form
  [<impl> <benchmark> (<cpu-msec> <real-msec> <gc-msec>) <compile-msec>]
where #f means that the information is unavailable, or that the
benchmark wasn't run due to an implementation limitation. The
<cpu-msec> and <compile-msec> parts are #f only when the benchmark
wasn't run.

All benchmarks must be run from the directory containing this file.

Most bechmarks were obtained from
 http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/code/bench/gabriel/
 http://www.ccs.neu.edu/home/will/GC/sourcecode.html
 Marc Feeley

Files that end in ".sch" are supposed to be standard Scheme plus `time'.
Files that end in ".ss" are MzScheme wrapper modules or helper scripts.

To build <benchmark>.sch directly with Gambit, Bigloo, or Chicken:
  mzscheme -qr mk-gambit.ss <benchmark> ; gsi -:m10000 <benchmark>.o1
  mzscheme -qr mk-bigloo.ss <benchmark> ; <benchmark>
  mzscheme -qr mk-chicken.ss <benchmark> ; <benchmark>

Unpack "dynamic-input.txt.gz" if you want to run the "dynamic" benchmark,
but the "auto.ss" script will do that for you.
