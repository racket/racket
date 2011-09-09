#lang typed/scheme

(require racket/flonum)

(define-syntax FLOAT*    (syntax-rules () ((FLOAT* x ...) (ann (* (ann x Float) ...) Float))))
(define-syntax FLOATsin  (syntax-rules () ((FLOATsin  x)  (ann (flsin (ann x Float)) Float))))

(: tfo-align Any)
(define (tfo-align) 0.0

 (let* ((x (FLOAT* 0.0 (FLOATsin 0.))))
   0))
