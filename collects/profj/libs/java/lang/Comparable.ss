(module Comparable scheme/base
  (require scheme/class)
  (provide (all-defined-out))
  (define Comparable (interface () compareTo-java.lang.Object)))
