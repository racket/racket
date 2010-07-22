(module invalid-binary-nonzero-fixnum typed/scheme
  (: f ( -> Void))
  (define f ; in a function, to prevent evaluation
    (lambda () (display (quotient 4 0))))) ; 2 fixnums, but the second is 0, cannot optimize
