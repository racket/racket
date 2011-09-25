#lang racket/base

;; The `first', etc. operations in this library
;;  work on pairs, not lists.

(require (only-in racket/list
                  cons?
                  empty?
                  empty
                  last-pair))

(provide first
         second
         third
         fourth
         fifth
         sixth
         seventh
         eighth

         rest

         cons?
         empty
         empty?

         foldl
         foldr

         last-pair

         remv
         remq
         remove
         remv*
         remq*
         remove*

         assf
         memf
         findf

         filter

         quicksort ; deprecated
         mergesort ; deprecated
         sort
         merge-sorted-lists)

;; a non-destructive version for symmetry with merge-sorted-lists!
(define (merge-sorted-lists a b less?)
  (cond [(null? a) b]
        [(null? b) a]
        [else (let loop ([x (car a)] [a (cdr a)] [y (car b)] [b (cdr b)])
                ;; The loop handles the merging of non-empty lists.  It has
                ;; been written this way to save testing and car/cdring.
                (if (less? y x)
                  (if (null? b)
                    (list* y x a)
                    (cons y (loop x a (car b) (cdr b))))
                  ;; x <= y
                  (if (null? a)
                    (list* x y b)
                    (cons x (loop (car a) (cdr a) y b)))))]))

;; deprecated!
(define quicksort sort)
(define mergesort sort)

(define (first x)
  (unless (pair? x) (raise-type-error 'first "non-empty list" x))
  (car x))
(define-syntax define-lgetter
  (syntax-rules ()
    [(_ name npos)
     (define (name l0)
       (let loop ([l l0] [pos npos])
         (if (pair? l)
           (if (eq? pos 1) (car l) (loop (cdr l) (sub1 pos)))
           (raise-type-error
            'name (format "list with ~a or more items" npos) l0))))]))
(define-lgetter second  2)
(define-lgetter third   3)
(define-lgetter fourth  4)
(define-lgetter fifth   5)
(define-lgetter sixth   6)
(define-lgetter seventh 7)
(define-lgetter eighth  8)

(define (rest x)
  (unless (pair? x)
    (raise-type-error 'rest "non-empty list" x))
  (cdr x))
