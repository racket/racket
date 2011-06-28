#lang racket
(require "status.rkt")

(define (timing? bs)
  (regexp-match #px#"cpu time: \\d+ real time: \\d+ gc time: \\d+" bs))

(define do-not-compare? timing?)

(define (find-next p? l)
  (match l
    [(list)
     (values)]
    [(list-rest (? p? e) l)
     (values e l)]
    [(list-rest n l)
     (call-with-values 
      (lambda ()
        (find-next p? l))
      (case-lambda
        [()
         (values)]
        [(e r)
         (values e (list* n r))]))]))

(define (same? b1 b2)
  (or (and (do-not-compare? b1) (do-not-compare? b2))
      (bytes=? b1 b2)))
(define (different? b1 b2)
  (not (same? b1 b2)))

(define (log-different? l1 l2)
  (let loop ([l1 l1] [l2 l2])
    (match l1
      [(list)
       (not (empty? l2))]
      [(list-rest e1 r1)
       (define (inner bs p? p-bytes)
         (call-with-values
          (lambda () (find-next p? l2))
          (case-lambda 
            [() #t]
            [(e2 r2)
             (or (different? bs (p-bytes e2))
                 (loop r1 r2))])))
       (match e1
         [(struct stdout (bs))
          (inner bs stdout? stdout-bytes)]
         [(struct stderr (bs))
          (inner bs stderr? stderr-bytes)])])))

(define-struct difference (old new))
(define-struct same-itude (e))

(define (render-log-difference l1 l2)
  (let loop ([l1 l1] [l2 l2])
    (match l1
      [(list)
       (map (match-lambda 
              [(? stdout? e2)
               (make-difference (make-stdout #"") e2)]
              [(? stderr? e2)
               (make-difference (make-stderr #"") e2)])
            l2)]
      [(list-rest e1 r1)
       (define (inner bs make-p p? p-bytes)
         (call-with-values
          (lambda () (find-next p? l2))
          (case-lambda 
            [()
             (list* (make-difference e1 (make-p #""))
                    (loop r1 l2))]
            [(e2 r2)
             (if (different? bs (p-bytes e2))
                 (list* (make-difference e1 e2)
                        (loop r1 r2))
                 (list* (make-same-itude e1)
                        (loop r1 r2)))])))
       (match e1
         [(struct stdout (bs))
          (inner bs make-stdout stdout? stdout-bytes)]
         [(struct stderr (bs))
          (inner bs make-stderr stderr? stderr-bytes)])])))

(provide (all-defined-out))
