#lang scheme
(require typed/scheme)

(with-type Number 3)

(let ([x "hello"])
  (with-type String
    #:freevars ([x String])
    (string-append x ", world")))
