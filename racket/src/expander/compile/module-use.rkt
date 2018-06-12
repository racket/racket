#lang racket/base
(require "../common/module-path.rkt")

;; A `module-use` record is just a part of module path index plus
;; phase, since that combination is commonly needed

(provide (struct-out module-use))

(struct module-use (module phase)
  #:property prop:equal+hash
  (list (lambda (a b eql?)
          (define a-mod (module-use-module a))
          (define b-mod (module-use-module b))
          (and (eql? a-mod b-mod)
               (eql? (module-use-phase a)
                     (module-use-phase b))
               ;; Unusual, but possible with top-level evaluation: can have
               ;; different "self" MPIs that refer to different modules
               (let-values ([(a-path a-base) (module-path-index-split a-mod)]
                            [(b-path b-base) (module-path-index-split b-mod)])
                 (or a-path
                     b-path
                     (eq? (module-path-index-resolved a-mod)
                          (module-path-index-resolved b-mod))))))
        (lambda (a hash-code)
          (+ (hash-code (module-use-module a))
             (hash-code (module-use-phase a))))
        (lambda (a hash-code)
          (+ (hash-code (module-use-module a))
             (hash-code (module-use-phase a))))))
