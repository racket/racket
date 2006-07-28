Bechmarks obtained from
 http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/code/bench/gabriel/
 http://www.ccs.neu.edu/home/will/GC/sourcecode.html

Files that end in ".sch" are supposed to be standard Scheme plus `time'.
Files that end in ".ss" are MzScheme wrapper modules or helper scripts.

To build <benchmark>.sch with Gambit, Bigloo, or Chicken:
  mzscheme -qr mk-gambit.ss <banchmark>
  mzscheme -qr mk-bigloo.ss <banchmark>
  mzscheme -qr mk-chicken.ss <banchmark>

Unpack "dynamic-input.txt.gz" if you want to run the "dynamic" benchmark.
