#lang typed/racket

(: emit-subroutines (-> (Listof String) Void))
(define (emit-subroutines code*)
  (exit 0)
  (for : Void ([code : String code*])
       (display code)))
