#lang racket/base

;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/
;;; Derived from the Chicken variant by Sven Hartrumpf

(require racket/cmdline racket/require (for-syntax racket/base)
         racket/place (only-in racket/fixnum make-shared-fxvector)
         (filtered-in (lambda (name) (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops))

(define-syntax-rule (**leaf? v) (fx= 1 (vector*-length v)))
(define-syntax-rule (**node? v) (fx= 3 (vector*-length v)))

(define-syntax leaf  (make-rename-transformer #'vector))
(define-syntax leaf? (make-rename-transformer #'**leaf?))
(define-syntax node  (make-rename-transformer #'vector))
(define-syntax node? (make-rename-transformer #'**node?))
(define-syntax-rule (leaf-val l)   (vector*-ref l 0))
(define-syntax-rule (node-left n)  (vector*-ref n 1))
(define-syntax-rule (node-right n) (vector*-ref n 2))

(define (make item d)
  (if (fx= d 0)
    (leaf item)
    (let ([item2 (fx* item 2)] [d2 (fx- d 1)])
      (node item (make (fx- item2 1) d2) (make item2 d2)))))

(define-syntax-rule (check s)
  (let loop ([t s] [acc 0])
    (let ([acc (fx+ (leaf-val t) acc)])
      (if (node? t)
        (loop (node-left t)
              (fx- acc (loop (node-right t) 0)))
        acc))))

(require racket/match)
(define (work c)
  (define args (place-channel-get c))
  (match-define (vector max-depth min-depth d) args)
  (define iterations (fxlshift 1 (fx+ (fx- max-depth d) min-depth)))
  (place-channel-put 
   c (vector (fx* 2 iterations) d
             (for/fold ([c 0]) ([i (in-range iterations)])
               (fx+ c (fx+ (check (make i d))
                           (check (make (fx- 0 i) d))))))))

(define min-depth 4)
(define (main n)
  (define max-depth (max (+ min-depth 2) n))
  (define stretch-depth (+ max-depth 1))
  (printf "stretch tree of depth ~a\t check: ~a\n"
          stretch-depth
          (check (make 0 stretch-depth)))
  (define len (fx+ max-depth 1))
  (define output (make-vector len #f))
  (define long-lived-tree (make 0 max-depth))
  (define thds
    (for/list ([d (in-range 4 len 2)])
      (thread (λ ()
                (define c (place ch (work ch)))
                (place-channel-put c (vector max-depth min-depth d))
                (vector-set! output d (place-channel-get c))))))
  (map sync thds)
  (for ([e (in-vector output)] #:when e)
    (printf "~a\t trees of depth ~a\t check: ~a\n"
            (vector-ref e 0) (vector-ref e 1) (vector-ref e 2)))
  (printf "long lived tree of depth ~a\t check: ~a\n"
          max-depth
          (check long-lived-tree)))

(module+ main (command-line #:args (n) (main (string->number n))))
