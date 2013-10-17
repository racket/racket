#lang racket/base

;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/
;;; Derived from the Chicken variant by Sven Hartrumpf

(require racket/cmdline racket/require (for-syntax racket/base)
         (filtered-in (lambda (name) (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops))

(struct leaf (val))
(struct node leaf (left right))

(define (make item d)
  (if (fx= d 0)
    (leaf item)
    (let ([item2 (fx* item 2)] [d2 (fx- d 1)])
      (node item (make (fx- item2 1) d2) (make item2 d2)))))

(define (check t)
  (let loop ([t t] [acc 0])
    (let ([acc (fx+ (leaf-val t) acc)])
      (if (node? t)
        (loop (node-left t)
              (fx- acc (loop (node-right t) 0)))
        acc))))

(define min-depth 4)

(define (main n)
  (let ([max-depth (max (+ min-depth 2) n)])
    (let ([stretch-depth (+ max-depth 1)])
      (printf "stretch tree of depth ~a\t check: ~a\n"
              stretch-depth
              (check (make 0 stretch-depth))))
    (let ([long-lived-tree (make 0 max-depth)])
      (for ([d (in-range 4 (+ max-depth 1) 2)])
        (let ([iterations (expt 2 (+ (- max-depth d) min-depth))])
          (printf "~a\t trees of depth ~a\t check: ~a\n"
                  (* 2 iterations)
                  d
                  (for/fold ([c 0]) ([i (in-range iterations)])
                    (fx+ c (fx+ (check (make i d))
                                (check (make (fx- 0 i) d))))))))
      (printf "long lived tree of depth ~a\t check: ~a\n"
              max-depth
              (check long-lived-tree)))))

(command-line #:args (n) (main (string->number n)))
