#;
(exn-pred 2)
#lang typed/scheme/base

(: gen-lambda-n-rest ((Any -> Any)
                      -> (Any -> (Any Any Any Any * -> Any))))
(define (gen-lambda-n-rest body)
  (error 'fail))

(: gen-lambda (Integer Any -> (Any -> (Any * -> Any))))
(define (gen-lambda nb-vars body)
  (case nb-vars
    ((3)  (gen-lambda-3 body))
    (else (gen-lambda-n nb-vars body))))

(: gen-lambda-3 (Any -> (Any -> (Any Any Any -> Any))))
(define (gen-lambda-3 body)
  (error 'fail))

(: gen-lambda-n (Integer Any -> (Any -> (Any Any Any Any * -> Any))))
(define (gen-lambda-n nb-vars body)
  (error 'fail))
