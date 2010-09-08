#;
(
#f line #f col #f - make-sequence - in-bytes
#f line #f col #f - (let-values (((pos->vals pos-next init pos-cont? val-cont? all-cont?) (#%app make-sequence (quote (i)) (quote 123)))) (#%app void) (#%app (letrec-values (((for-loop) (lambda (fold-var pos) (if (#%app pos-cont? pos) (let-values (((i) (#%app pos->vals pos))) (if (#%app val-cont? i) (let-values (((fold-var) (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))))) (if (#%app all-cont? pos i) (#%app for-loop fold-var (#%app pos-next pos)) fold-var)) fold-var)) fold-var)))) for-loop) (#%app void) init)) - unboxed let bindings
#f line #f col #f - (let-values (((i) (#%app pos->vals pos))) (if (#%app val-cont? i) (let-values (((fold-var) (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))))) (if (#%app all-cont? pos i) (#%app for-loop fold-var (#%app pos-next pos)) fold-var)) fold-var)) - unboxed let bindings
#f line #f col #f - (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))) - unboxed let bindings
#f line #f col #f - (let-values () (let-values () (#%app display i)) (#%app void)) - unboxed let bindings
#f line #f col #f - (let-values () (#%app display i)) - unboxed let bindings
#f line #f col #f - (let-values (((fold-var) (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))))) (if (#%app all-cont? pos i) (#%app for-loop fold-var (#%app pos-next pos)) fold-var)) - unboxed let bindings
in-bytes.rkt line 22 col 0 - (letrec-values (((for-loop) (lambda (fold-var pos) (if (#%app pos-cont? pos) (let-values (((i) (#%app pos->vals pos))) (if (#%app val-cont? i) (let-values (((fold-var) (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))))) (if (#%app all-cont? pos i) (#%app for-loop fold-var (#%app pos-next pos)) fold-var)) fold-var)) fold-var)))) for-loop) - unboxed let bindings
#f line #f col #f - (let-values (((i) (#%app pos->vals pos))) (if (#%app val-cont? i) (let-values (((fold-var) (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))))) (if (#%app all-cont? pos i) (#%app for-loop fold-var (#%app pos-next pos)) fold-var)) fold-var)) - unboxed let bindings
#f line #f col #f - (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))) - unboxed let bindings
#f line #f col #f - (let-values () (let-values () (#%app display i)) (#%app void)) - unboxed let bindings
#f line #f col #f - (let-values () (#%app display i)) - unboxed let bindings
#f line #f col #f - (let-values (((fold-var) (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))))) (if (#%app all-cont? pos i) (#%app for-loop fold-var (#%app pos-next pos)) fold-var)) - unboxed let bindings
in-bytes.rkt line 22 col 0 - (letrec-values (((for-loop) (lambda (fold-var pos) (if (#%app pos-cont? pos) (let-values (((i) (#%app pos->vals pos))) (if (#%app val-cont? i) (let-values (((fold-var) (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))))) (if (#%app all-cont? pos i) (#%app for-loop fold-var (#%app pos-next pos)) fold-var)) fold-var)) fold-var)))) for-loop) - unboxed let bindings
495051)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(for: ((i : Integer #"123"))
      (display i))
