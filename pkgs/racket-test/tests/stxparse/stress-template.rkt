#lang racket/base
(require syntax/parse/experimental/template)
(provide (all-defined-out))

(module test racket/base)

(define (f1-stx stx)
  (syntax-case stx ()
    [(_ body)
     #'(discard-exn body #f)]
    [(_ body on-exn)
     #'(with-handlers ([exn:fail? (lambda (_) on-exn)])
         body)]))

(define (f1-tmpl stx)
  (syntax-case stx ()
    [(_ body)
     (template (discard-exn body #f))]
    [(_ body on-exn)
     (template (with-handlers ([exn:fail? (lambda (_) on-exn)])
                 body))]))

(define (f2-stx stx)
  (syntax-case stx ()
    [(_ (x ...) (y ...) z)
     #'((x z) ... ((y x) ... z))]))

(define (f2-tmpl stx)
  (syntax-case stx ()
    [(_ (x ...) (y ...) z)
     (template ((x z) ... ((y x) ... z)))]))

(define (f3-stx stx)
  (syntax-case stx ()
    [(_ (x ...) (y ...) z)
     #'((x 1) ... ((y 2) ... z))]))

(define (f3-tmpl stx)
  (syntax-case stx ()
    [(_ (x ...) (y ...) z)
     (template ((x 1) ... ((y 2) ... z)))]))

(define (f4-stx stx)  ;; test common ellipsis case
  (syntax-case stx ()
    [(_ (x ...) (y ...) z)
     #'(blah (x ...) (y ...) z)]))

(define (f4-tmpl stx)
  (syntax-case stx ()
    [(_ (x ...) (y ...) z)
     (template (blah (x ...) (y ...) z))]))

(define (test f term)
  (collect-garbage)
  (time (void (for ([i #e1e5]) (f term)))))

(define stx2a
  #`(_
     #,(for/list ([i 10]) i)
     #,(for/list ([i 10]) 'a)
     z))

(define stx2
  #`(_
     #,(for/list ([i 100]) i)
     #,(for/list ([i 100]) 'a)
     z))

(define progss
  '(((test f1-stx #'(_ e))
     (test f1-tmpl #'(_ e)))

    ((test f2-stx stx2a)
     (test f2-tmpl stx2a))

    ((test f2-stx stx2)
     (test f2-tmpl stx2))

    ((test f3-stx stx2a)
     (test f3-tmpl stx2a))

    ((test f3-stx stx2)
     (test f3-tmpl stx2))

    ((test f4-stx stx2a)
     (test f4-tmpl stx2a))

    ((test f4-stx stx2)
     (test f4-tmpl stx2))))

(define-namespace-anchor nsa)

(for ([progs progss])
  (for ([p progs])
    (printf "> ~s\n" p)
    (eval p (namespace-anchor->namespace nsa)))
  (newline))
