;;; The Great Computer Language Shootout
;;; http://shootout.alioth.debian.org/
;;; Derived from the Chicken variant by Sven Hartrumpf
#lang scheme/base
(require scheme/cmdline)

(define-struct node (left val right))

;; Instead of (define-struct leaf (val)):
(define (make-leaf val) (make-node #f val #f))
(define (leaf? l) (not (node-left l)))
(define (leaf-val l) (node-val l))

(define (make item d)
  (if (= d 0)
      (make-leaf item)
      (let ((item2 (* item 2))
            (d2 (- d 1)))
        (make-node (make (- item2 1) d2) item (make item2 d2)))))

(define (check t)
  (if (leaf? t)
      (leaf-val t)
      (+ (node-val t) (- (check (node-left t)) 
                         (check (node-right t))))))

(define (main n)
  (let* ((min-depth 4)
         (max-depth (max (+ min-depth 2) n)))
    (let ((stretch-depth (+ max-depth 1)))
      (printf "stretch tree of depth ~a\t check: ~a\n"
              stretch-depth
              (check (make 0 stretch-depth))))
    (let ((long-lived-tree (make 0 max-depth)))
      (for ((d (in-range 4 (add1 max-depth) 2)))
        (let ((iterations (arithmetic-shift 1 (+ (- max-depth d) min-depth))))
          (printf "~a\t trees of depth ~a\t check: ~a\n"
                  (* 2 iterations)
                  d
                  (for/fold ([c 0])
                            ([i (in-range iterations)])
                    (+ c 
                       (check (make i d)) 
                       (check (make (- i) d)))))))
      (printf "long lived tree of depth ~a\t check: ~a\n"
              max-depth
              (check long-lived-tree)))))

(command-line #:args (n) 
              (main (string->number n)))
