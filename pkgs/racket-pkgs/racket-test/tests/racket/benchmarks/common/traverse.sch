;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         traverse.sch
; Description:  TRAVERSE benchmark
; Author:       Richard Gabriel
; Created:      12-Apr-85
; Modified:     12-Apr-85 10:24:04 (Bob Shaw)
;               9-Aug-87 (Will Clinger)
; Language:     Scheme (but see note)
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note:  This benchmark may depend upon the empty list being the same
; as #f.
 
;;; TRAVERSE --  Benchmark which creates and traverses a tree structure.

(define (make-node)
  (let ((node (make-vector 11 '())))
    (vector-set! node 0 'node)
    (vector-set! node 3 (snb))
    node))

(define (node-parents node) (vector-ref node 1))
(define (node-sons node) (vector-ref node 2))
(define (node-sn node) (vector-ref node 3))
(define (node-entry1 node) (vector-ref node 4))
(define (node-entry2 node) (vector-ref node 5))
(define (node-entry3 node) (vector-ref node 6))
(define (node-entry4 node) (vector-ref node 7))
(define (node-entry5 node) (vector-ref node 8))
(define (node-entry6 node) (vector-ref node 9))
(define (node-mark node) (vector-ref node 10))

(define (node-parents-set! node v) (vector-set! node 1 v))
(define (node-sons-set! node v) (vector-set! node 2 v))
(define (node-sn-set! node v) (vector-set! node 3 v))
(define (node-entry1-set! node v) (vector-set! node 4 v))
(define (node-entry2-set! node v) (vector-set! node 5 v))
(define (node-entry3-set! node v) (vector-set! node 6 v))
(define (node-entry4-set! node v) (vector-set! node 7 v))
(define (node-entry5-set! node v) (vector-set! node 8 v))
(define (node-entry6-set! node v) (vector-set! node 9 v))
(define (node-mark-set! node v) (vector-set! node 10 v))

(define *sn* 0)
(define *rand* 21)
(define *count* 0)
(define *marker* #f)
(define *root* '())

(define (snb)
  (set! *sn* (+ 1 *sn*))
  *sn*)
 
(define (seed)
  (set! *rand* 21)
  *rand*)
 
(define (traverse-random)
  (set! *rand* (remainder (* *rand* 17) 251))
  *rand*)
 
(define (traverse-remove n q)
  (cond ((eq? (cdr (car q)) (car q))
         (let ((x (caar q))) (set-car! q #f) x))
        ((zero? n)
         (let ((x (caar q)))
           (do ((p (car q) (cdr p)))
               ((eq? (cdr p) (car q))
                (set-cdr! p (cdr (car q)))
                (set-car! q p)))
           x))
        (else (do ((n n (- n 1))
                   (q (car q) (cdr q))
                   (p (cdr (car q)) (cdr p)))
                  ((zero? n) (let ((x (car q))) (set-cdr! q p) x))))))
 
(define (traverse-select n q)
  (do ((n n (- n 1))
       (q (car q) (cdr q)))
      ((zero? n) (car q))))
 
(define (add a q)
  (cond ((null? q)
         `(,(let ((x `(,a)))
              (set-cdr! x x) x)))
        ((null? (car q))
         (let ((x `(,a)))
           (set-cdr! x x)
           (set-car! q x)
           q))
        ; the CL version had a useless set-car! in the next line (wc)
        (else (set-cdr! (car q) `(,a . ,(cdr (car q))))
              q)))
 
(define (create-structure n)
  (let ((a `(,(make-node))))
    (do ((m (- n 1) (- m 1))
         (p a))
        ((zero? m)
         (set! a `(,(begin (set-cdr! p a) p)))
         (do ((unused a)
              (used (add (traverse-remove 0 a) #f))
              (x 0)
              (y 0))
             ((null? (car unused))
              (find-root (traverse-select 0 used) n))
           (set! x (traverse-remove (remainder (traverse-random) n) unused))
           (set! y (traverse-select (remainder (traverse-random) n) used))
           (add x used)
           (node-sons-set! y `(,x . ,(node-sons y)))
           (node-parents-set! x `(,y . ,(node-parents x))) ))
      (set! a (cons (make-node) a)))))
 
(define (find-root node n)
  (do ((n n (- n 1)))
      ((or (zero? n) (null? (node-parents node)))
       node)
    (set! node (car (node-parents node)))))
 
(define (travers node mark)
  (cond ((eq? (node-mark node) mark) #f)
        (else (node-mark-set! node mark)
           (set! *count* (+ 1 *count*))
           (node-entry1-set! node (not (node-entry1 node)))
           (node-entry2-set! node (not (node-entry2 node)))
           (node-entry3-set! node (not (node-entry3 node)))
           (node-entry4-set! node (not (node-entry4 node)))
           (node-entry5-set! node (not (node-entry5 node)))
           (node-entry6-set! node (not (node-entry6 node)))
           (do ((sons (node-sons node) (cdr sons)))
               ((null? sons) #f)
             (travers (car sons) mark)))))
 
(define (traverse root)
  (let ((*count* 0))
    (travers root (begin (set! *marker* (not *marker*)) *marker*))
    *count*))
 
(define (init-traverse)  ; Changed from defmacro to defun \bs
  (set! *root* (create-structure 100))
  #f)
 
(define (run-traverse)  ; Changed from defmacro to defun \bs
  (do ((i 50 (- i 1)))
      ((zero? i))
    (traverse *root*)
    (traverse *root*)
    (traverse *root*)
    (traverse *root*)
    (traverse *root*)))
 
;;; to initialize, call:  (init-traverse)
;;; to run traverse, call:  (run-traverse)
 
(time (init-traverse))
(time (run-traverse))

