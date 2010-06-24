#lang racket
(require racketunit
         "../ast.rkt"
         "util.rkt"
         "../sexp.rkt")

(provide sexp-tests)

(define test
  #'(begin
      (! (parent john douglas))
      (? (parent john douglas))
      (? (parent john ebbon))
      (! (parent bob john))
      (! (parent ebbon bob))
      (? (parent ,A ,B))
      (? (parent john ,B))
      (? (parent ,A ,A))
      (! (:- (ancestor ,A ,B)
             (parent ,A ,B)))
      (! (:- (ancestor ,A ,B)
             (parent ,A ,C)
             (ancestor ,C ,B)))
      (? (ancestor ,A ,B))
      (? (ancestor ,X john))
      (~ (parent bob john))
      (? (parent ,A ,B))
      (? (ancestor ,A ,B))))

(define sexp-tests
  (test-suite
   "sexp"
   
   (test-not-exn "program" (lambda () (contract program/c (stx->program test) 'pos 'neg)))
   
   (test-not-false "stmt" (assertion? (stx->statement #'(! (parent john douglas)))))
   (test-not-false "stmt" (retraction? (stx->statement #'(~ (parent john douglas)))))
   (test-not-false "stmt" (query? (stx->statement #'(? (parent john douglas)))))
   
   (test-clause "clause" (stx->clause #'(parent john douglas))
                (make-clause #f (make-literal #f 'parent (list (make-constant #f 'john) (make-constant #f 'douglas))) empty))
   (test-clause "clause" (stx->clause #'(:- (ancestor ,A ,B) (parent ,A ,B)))
                (make-clause #f (make-literal #f 'ancestor (list (make-variable #f 'A) (make-variable #f 'B)))
                             (list (make-literal #f 'parent (list (make-variable #f 'A) (make-variable #f 'B))))))
   
   (test-literal "literal" (stx->literal #'(parent john douglas))
                 (make-literal #f 'parent (list (make-constant #f 'john) (make-constant #f 'douglas))))
   
   ))