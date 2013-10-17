To run a benchmark, assuming you have `racket' in your path:
 ./auto.rkt <impl-or-benchmark> ...
where <impl-or-benchmark> names an implementation as one of
   racket
   bigloo
   chicken
   gambit
   larceny
   ...
or a benchmark as one of
   conform
   cpstak
   ctak
   ...
or any of the above prefixed by "no-" to skip the corresponding
<impl-or-benchmark>. To see a complete list of implementations
and benchmarks, run
 ./auto.rkt --show

Naming no implementation/benchmark causes a standard set of them to be
run (as reported by --show). Similarly, if the first named
implementation/benchmak starts with "no-", the default set is used
minus the "no-"-specified implementation/benchmark.

The output is a series of lines of the form
  [<impl> <benchmark> (<cpu-msec> <real-msec> <gc-msec>) <compile-msec>]
where #f means that the information is unavailable, or that the
benchmark wasn't run due to an implementation limitation. The
<cpu-msec> and <compile-msec> parts are #f only when the benchmark
wasn't run.

Most bechmarks were obtained from
 http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/code/bench/gabriel/
 http://www.ccs.neu.edu/home/will/GC/sourcecode.html
 Marc Feeley

Files that end in ".sch" are supposed to be standard Scheme plus `time'.
Files that end in ".rkt" are Racket wrapper modules or helper scripts.
Files that end in "-typed.rktl" are Typed Scheme versions of the benchmarks.
Files that end in "-[non-]optimizing.rkt" are Typed Scheme wrappers
that turn Typed Scheme's optimizer on or off.

To build <benchmark>.sch directly with Gambit, Bigloo, or Chicken:
  racket -f mk-gambit.rktl <benchmark> ; gsi -:m10000 <benchmark>.o1
  racket -f mk-bigloo.rktl <benchmark> ; <benchmark>
  racket -f mk-chicken.rktl <benchmark> ; <benchmark>

Unpack "dynamic-input.txt.gz" if you want to run the "dynamic" benchmark,
but the "auto.rkt" script will do that for you.
