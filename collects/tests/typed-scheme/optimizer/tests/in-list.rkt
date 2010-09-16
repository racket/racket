#;
(
#f line #f col #f - make-sequence - in-list
#f line #f col #f - (let-values (((pos->vals pos-next init pos-cont? val-cont? all-cont?) (#%app make-sequence (quote (i)) (quote (1 2 3))))) (#%app void) (#%app (letrec-values (((for-loop) (lambda (fold-var pos) (if (#%expression (#%app pos-cont? pos)) (let-values (((i) (#%app pos->vals pos))) (if (#%expression (#%app val-cont? i)) (let-values (((fold-var) (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))))) (if (#%expression (#%app all-cont? pos i)) (#%app for-loop fold-var (#%app pos-next pos)) fold-var)) fold-var)) fold-var)))) for-loop) (#%app void) init)) - unboxed let bindings
#f line #f col #f - (let-values (((i) (#%app pos->vals pos))) (if (#%expression (#%app val-cont? i)) (let-values (((fold-var) (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))))) (if (#%expression (#%app all-cont? pos i)) (#%app for-loop fold-var (#%app pos-next pos)) fold-var)) fold-var)) - unboxed let bindings
#f line #f col #f - (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))) - unboxed let bindings
#f line #f col #f - (let-values () (let-values () (#%app display i)) (#%app void)) - unboxed let bindings
#f line #f col #f - (let-values () (#%app display i)) - unboxed let bindings
#f line #f col #f - (let-values (((fold-var) (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))))) (if (#%expression (#%app all-cont? pos i)) (#%app for-loop fold-var (#%app pos-next pos)) fold-var)) - unboxed let bindings
in-list.rkt line 22 col 0 - (letrec-values (((for-loop) (lambda (fold-var pos) (if (#%expression (#%app pos-cont? pos)) (let-values (((i) (#%app pos->vals pos))) (if (#%expression (#%app val-cont? i)) (let-values (((fold-var) (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))))) (if (#%expression (#%app all-cont? pos i)) (#%app for-loop fold-var (#%app pos-next pos)) fold-var)) fold-var)) fold-var)))) for-loop) - unboxed let bindings
#f line #f col #f - (let-values (((i) (#%app pos->vals pos))) (if (#%expression (#%app val-cont? i)) (let-values (((fold-var) (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))))) (if (#%expression (#%app all-cont? pos i)) (#%app for-loop fold-var (#%app pos-next pos)) fold-var)) fold-var)) - unboxed let bindings
#f line #f col #f - (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))) - unboxed let bindings
#f line #f col #f - (let-values () (let-values () (#%app display i)) (#%app void)) - unboxed let bindings
#f line #f col #f - (let-values () (#%app display i)) - unboxed let bindings
#f line #f col #f - (let-values (((fold-var) (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))))) (if (#%expression (#%app all-cont? pos i)) (#%app for-loop fold-var (#%app pos-next pos)) fold-var)) - unboxed let bindings
in-list.rkt line 22 col 0 - (letrec-values (((for-loop) (lambda (fold-var pos) (if (#%expression (#%app pos-cont? pos)) (let-values (((i) (#%app pos->vals pos))) (if (#%expression (#%app val-cont? i)) (let-values (((fold-var) (let-values (((fold-var) fold-var)) (let-values () (let-values () (#%app display i)) (#%app void))))) (if (#%expression (#%app all-cont? pos i)) (#%app for-loop fold-var (#%app pos-next pos)) fold-var)) fold-var)) fold-var)))) for-loop) - unboxed let bindings
123)

#lang typed/scheme
#:optimize

(for: ((i : Natural '(1 2 3)))
      (display i))
