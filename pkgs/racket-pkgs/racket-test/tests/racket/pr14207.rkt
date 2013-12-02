#lang racket/base
(let ()
  (local-require (rename-in racket/base [string rkt:strrng]))
  ;;=> 3:28: syntax: misplaced ellipsis in template at: ... in: ...
  rkt:strrng)
