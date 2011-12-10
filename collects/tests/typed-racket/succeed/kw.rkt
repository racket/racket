#lang typed/scheme

(lambda ()
  (open-input-file "foo" #:mode 'binary)
  (open-input-file "foo" #:mode 'text)
  (open-input-file "foo"))

((inst sort Real Real) (list 1 2 3) >)

((inst sort Real Real) (list 1 2 3) #:key (λ: ([x : Real]) (/ 1 x)) >)

((inst sort Real String) (list 1 2 3) #:key number->string string<?)

((inst sort Real String) (list 1 2 3) #:key number->string string<? #:cache-keys? #t)

(remove-duplicates  '("foo"))

(sort (list 1 2 3) >)
