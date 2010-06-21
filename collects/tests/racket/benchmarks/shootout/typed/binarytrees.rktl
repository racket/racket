;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/
;;; Derived from the Chicken variant by Sven Hartrumpf

(require racket/cmdline)

(define-struct: node ((left : (Option node)) (val : Integer) (right : (Option node))))

;; Instead of (define-struct leaf (val)):
(: leaf (Integer -> node))
(define (leaf val) (node #f val #f))
(: leaf? (node -> Boolean))
(define (leaf? l) (not (node-left l)))
(: leaf-val (node -> Integer))
(define (leaf-val l) (node-val l))

(: make (Integer Integer -> node))
(define (make item d)
  (if (= d 0)
      (leaf item)
      (let ((item2 (* item 2))
            (d2 (- d 1)))
        (node (make (- item2 1) d2) 
              item 
              (make item2 d2)))))

(: check (node -> Integer))
(define (check t)
  (if (leaf? t)
      (leaf-val t)
      (+ (node-val t) (- (check (assert (node-left t))) 
                         (check (assert (node-right t)))))))

(: main (Integer -> Void))
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
                  (for/fold: : Integer ([c : Integer 0])
                             ([i : Integer (in-range iterations)])
                    (+ c 
                       (check (make i d)) 
                       (check (make (- i) d)))))))
      (printf "long lived tree of depth ~a\t check: ~a\n"
              max-depth
              (check long-lived-tree)))))

(command-line #:args (n) 
              (main (assert (string->number (assert n string?)) exact-integer?)))
