#cs
(module Comparable mzscheme
  (require (lib "class.ss"))
  (provide (all-defined))
  (define Comparable (interface () compareTo-java.lang.Object)))
