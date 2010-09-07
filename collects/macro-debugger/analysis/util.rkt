#lang racket/base
(require syntax/modcode
         syntax/modresolve
         macro-debugger/model/trace)

(provide get-module-code/trace
         here-mpi?)

;; get-module-derivation : module-path -> (values compiled Deriv)
(define (get-module-code/trace path)
  (get-module-code (resolve-module-path path #f)
                   #:choose (lambda _ 'src)
                   #:compile (lambda (stx)
                               (let-values ([(stx deriv) (trace/result stx expand)])
                                 (values (compile stx) deriv)))))

;; here-mpi? : any -> boolean
(define (here-mpi? x)
  (and (module-path-index? x)
       (let-values ([(rel base) (module-path-index-split x)])
         (and (eq? rel #f) (eq? base #f)))))
