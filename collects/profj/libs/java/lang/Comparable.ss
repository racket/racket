#cs
(module Comparable mzscheme
  (require mzlib/class)
  (provide (all-defined))
  (define Comparable (interface () compareTo-java.lang.Object)))
