This is the model from

  A List-machine Benchmark for Mechanized Metatheory
  Andrew W. Appel, Robert Dockins, and Xavier Leroy

Specifically, it contains all of the formal definitions in sections 3
and 4 (list-machine.rkt), section 5 (list-machine-typing.rkt), the
example from section 6 (sample.rkt), and the lub definition in 8.1
(also in list-machine-typing.rkt).

All of the relations and functions are directly executable in Redex
(using the lub definition from 8.1, not the one in 5).

The file test.rkt contains unit tests, roughly one per case in each of
the definitions, and the file slides.rkt is a Slideshow presentation
that shows the instruction reduction relation and the type system.
