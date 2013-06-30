#lang racket/base

(provide (struct-out nt)
         (struct-out rhs)
         the-not-hole
         the-hole
         hole?)

;; lang = (listof nt)
;; nt = (make-nt sym (listof rhs))
;; rhs = (make-rhs single-pattern)
;; single-pattern = sexp
(define-struct nt (name rhs) #:transparent)
(define-struct rhs (pattern) #:transparent)
(define-values (the-hole the-not-hole hole?)
  (let ()
    (struct hole (which)
      #:property prop:equal+hash (list (λ (x y recur) #t)
                                       (λ (v recur) 255)
                                       (λ (v recur) 65535))
      #:methods gen:custom-write
      [(define (write-proc a-hole port mode)
         (define str (if (equal? (hole-which a-hole) 'the-hole)
                         "hole"
                         "not-hole"))
         (cond
           [(or (equal? mode 0) (equal? mode 1))
            (write-string str port)]
           [else
            (write-string "#<" port)
            (write-string str port)
            (write-string ">" port)]))]
      #:inspector #f)
    (define the-hole (hole 'the-hole))
    (define the-not-hole (hole 'the-not-hole))
    (values the-hole the-not-hole hole?)))
