To run a benchmark:
 mzscheme -qu auto.ss <impl-or-benchmark> ...
where <impl-or-benchmark> names an implementation as one of
   mzscheme3m
   bigloo
   chicken
   gambit
   larceny
   mzscheme    [omitted by default]
   mzscheme-j  [omitted by default]
   mzscheme-tl [omitted by default]
or a benchmark as one of
   conform
   cpstack
   ctak
   ...
or any of the above prefixed by "no-" to skip the corresponding
<impl-or-benchmark>.

Naming no implementation/benchmark causes all of them to be run,
except ones omitted by default. Similarly, if the first named
implementation/benchmak starts with "no-", the default set is used
minus the "no-"-specified implementation/benchmark.

The output is series of lines of the form
  [<impl> <benchmark> (<cpu-msec> <real-msec> <gc-msec>) <compile-msec>]

Most bechmarks were obtained from
 http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/code/bench/gabriel/
 http://www.ccs.neu.edu/home/will/GC/sourcecode.html

Files that end in ".sch" are supposed to be standard Scheme plus `time'.
Files that end in ".ss" are MzScheme wrapper modules or helper scripts.

To build <benchmark>.sch directly with Gambit, Bigloo, or Chicken:
  mzscheme -qr mk-gambit.ss <banchmark>
  mzscheme -qr mk-bigloo.ss <banchmark>
  mzscheme -qr mk-chicken.ss <banchmark>

Unpack "dynamic-input.txt.gz" if you want to run the "dynamic" benchmark.
