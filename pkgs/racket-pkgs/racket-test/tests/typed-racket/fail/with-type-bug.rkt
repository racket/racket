#;
(exn-pred exn:fail:contract?)
#lang scheme
(require (prefix-in T: typed/scheme))
((T:with-type #:result (T:Integer T:-> T:Integer) add1) 1/2)
