#lang racket/base
(provide (struct-out arguments)
         (struct-out arity)
         no-arguments
         no-arity
         to-procedure-arity
         arguments->arity
         check-arity
         check-curry
         join-sep
         kw->string
         diff/sorted/eq)

#|
An Arguments is
  #s(arguments (listof stx) (listof keyword) (listof stx))
|#
(define-struct arguments (pargs kws kwargs) #:prefab)

(define no-arguments (arguments null null null))

#|
An Arity is
  #s(arity nat nat/+inf.0 (listof keyword) (listof keyword))
|#
(define-struct arity (minpos maxpos minkws maxkws)
  #:prefab)

(define no-arity (arity 0 0 null null))

;; ----

(define (to-procedure-arity minpos maxpos)
  (cond [(= minpos maxpos) minpos]
        [(= maxpos +inf.0) (arity-at-least minpos)]
        [else (for/list ([i (in-range minpos (add1 maxpos))]) i)]))

(define (arguments->arity argu)
  (let ([pos (length (arguments-pargs argu))]
        [kws (arguments-kws argu)])
    (arity pos pos kws kws)))

(define (check-arity arity pos-count keywords0 proc)
  (define keywords (sort keywords0 keyword<?))
  (define minpos (arity-minpos arity))
  (define maxpos (arity-maxpos arity))
  (define minkws (arity-minkws arity))
  (define maxkws (arity-maxkws arity))
  (unless (<= minpos pos-count maxpos)
    (proc (format "syntax class arity mismatch~a\n  expected: ~a\n  given: ~a"
                  ";\n the expected number of arguments does not match the given number"
                  (gen-expected-msg minpos maxpos minkws maxkws)
                  (gen-given-msg pos-count keywords))))
  (let ([missing-kws (diff/sorted/eq minkws keywords)])
    (unless (null? missing-kws)
      (proc (format "syntax class required keyword argument~a not supplied\n  required: ~a"
                    (s-if-plural missing-kws)
                    (join-sep (map kw->string missing-kws) "," "and")))))
  (let ([extra-kws (diff/sorted/eq keywords maxkws)])
    (unless (null? extra-kws)
      (proc (format "syntax class does not expect given keyword argument~a\n  given: ~a"
                    (s-if-plural extra-kws)
                    (join-sep (map kw->string extra-kws) "," "and"))))))

(define (gen-expected-msg minpos maxpos minkws maxkws)
  (define pos-part
    (cond [(= minpos maxpos) (format "~s" minpos)]
          [(eqv? maxpos +inf.0) (format "at least ~s" minpos)]
          [else (format "between ~s and ~s" minpos maxpos)]))
  (define kws-part
    (cond [(pair? minkws)
           (format " plus keyword argument~a ~a"
                   (s-if-plural minkws)
                   (join-sep (map kw->string minkws) "," "and"))]
          [else ""]))
  (define optkws (diff/sorted/eq maxkws minkws))
  (define optkws-part
    (cond [(pair? optkws)
           (format " plus optional keyword argument~a ~a"
                   (s-if-plural optkws)
                   (join-sep (map kw->string minkws) "," "and"))]
          [else ""]))
  (string-append pos-part kws-part optkws-part))

(define (gen-given-msg pos-count kws)
  (define kws-part
    (cond [(pair? kws)
           (format " plus keyword argument~a ~a"
                   (s-if-plural kws)
                   (join-sep (map kw->string kws) "," "and"))]
          [else ""]))
  (format "~s~a" pos-count kws-part))

;; ----

(define (check-curry arity pos-count keywords proc)
  (let ([maxpos (arity-maxpos arity)]
        [maxkws (arity-maxkws arity)])
    (when (> pos-count maxpos)
      (proc (format "too many arguments\n  expected: at most ~s\n  given: ~s"
                    maxpos pos-count)))
    (let ([extrakws (diff/sorted/eq keywords maxkws)])
      (when (pair? extrakws)
        (proc (format "syntax class does not expect given keyword arguments\n  given keywords: ~a"
                      (join-sep (map kw->string extrakws) "," "and")))))))

;; ----

(define (kw->string kw) (format "~a" kw))

(define (diff/sorted/eq xs ys)
  (if (pair? xs)
      (let ([ys* (memq (car xs) ys)])
        (if ys*
            (diff/sorted/eq (cdr xs) (cdr ys*))
            (cons (car xs) (diff/sorted/eq (cdr xs) ys))))
      null))

(define (join-sep items sep0 ult0 [prefix ""])
  (define sep (string-append sep0 " "))
  (define ult (string-append ult0 " "))
  (define (loop items)
    (cond [(null? items)
           null]
          [(null? (cdr items))
           (list sep ult (car items))]
          [else
           (list* sep (car items) (loop (cdr items)))]))
  (case (length items)
    [(0) #f]
    [(1) (string-append prefix (car items))]
    [(2) (format "~a~a ~a~a" prefix (car items) ult (cadr items))]
    [else (let ([strings (list* (car items) (loop (cdr items)))])
            (apply string-append prefix strings))]))

(define (s-if-plural xs) (if (= (length xs) 1) "" "s"))
