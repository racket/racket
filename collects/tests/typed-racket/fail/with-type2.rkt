#;
(exn-pred exn:fail:contract?)
#lang scheme

(require typed/scheme)

(let ([x 'hello])
  (with-type
   #:result String
   #:freevars ([x String])
   (string-append x ", world")))
