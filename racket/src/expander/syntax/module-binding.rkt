#lang racket/base
(require "../compile/serialize-property.rkt"
         "full-binding.rkt")

(provide make-module-binding
         module-binding-update
         module-binding?
         
         module-binding-module
         module-binding-phase
         module-binding-sym
         module-binding-nominal-module
         module-binding-nominal-phase
         module-binding-nominal-sym
         module-binding-nominal-require-phase
         module-binding-extra-inspector
         module-binding-extra-nominal-bindings
         
         deserialize-full-module-binding
         deserialize-simple-module-binding)

;; ----------------------------------------

(define (make-module-binding module phase sym
                             #:nominal-module [nominal-module module]
                             #:nominal-phase [nominal-phase phase]
                             #:nominal-sym [nominal-sym sym]
                             #:nominal-require-phase [nominal-require-phase 0]
                             #:frame-id [frame-id #f]
                             #:free=id [free=id #f]
                             #:extra-inspector [extra-inspector #f]
                             #:extra-nominal-bindings [extra-nominal-bindings null])
  (cond
   [(or frame-id
        free=id
        extra-inspector
        (not (and (eqv? nominal-phase phase)
                  (eq? nominal-sym sym)
                  (eqv? nominal-require-phase 0)
                  (null? extra-nominal-bindings))))
    (full-module-binding frame-id
                         free=id
                         module phase sym
                         nominal-module nominal-phase nominal-sym
                         nominal-require-phase
                         extra-inspector
                         extra-nominal-bindings)]
   [else
    (simple-module-binding module phase sym nominal-module)]))

(define (module-binding-update b
                               #:module [module (module-binding-module b)]
                               #:phase [phase (module-binding-phase b)]
                               #:sym [sym (module-binding-sym b)]
                               #:nominal-module [nominal-module (module-binding-nominal-module b)]
                               #:nominal-phase [nominal-phase (module-binding-nominal-phase b)]
                               #:nominal-sym [nominal-sym (module-binding-nominal-sym b)]
                               #:nominal-require-phase [nominal-require-phase (module-binding-nominal-require-phase b)]
                               #:frame-id [frame-id (binding-frame-id b)]
                               #:free=id [free=id (binding-free=id b)]
                               #:extra-inspector [extra-inspector (module-binding-extra-inspector b)]
                               #:extra-nominal-bindings [extra-nominal-bindings (module-binding-extra-nominal-bindings b)])
  (make-module-binding module phase sym
                       #:nominal-module nominal-module
                       #:nominal-phase nominal-phase
                       #:nominal-sym nominal-sym
                       #:nominal-require-phase nominal-require-phase
                       #:frame-id frame-id
                       #:free=id free=id
                       #:extra-inspector extra-inspector
                       #:extra-nominal-bindings extra-nominal-bindings))

(define (module-binding? b)
  ;; must not overlap with `local-binding?`
  (or (simple-module-binding? b)
      (full-module-binding? b)))

;; See `identifier-binding` docs for information about these fields:
(struct full-module-binding full-binding (module phase sym
                                           nominal-module nominal-phase nominal-sym
                                           nominal-require-phase
                                           extra-inspector ; preserves access to protected definitions
                                           extra-nominal-bindings)
  #:authentic
  #:transparent
  #:property prop:serialize
  (lambda (b ser-push! state)
    ;; Dropping the frame id may simplify the representation:
    (define simplified-b
      (if (full-binding-frame-id b)
          (module-binding-update b #:frame-id #f)
          b))
    (cond
      [(full-module-binding? simplified-b)
       (ser-push! 'tag '#:module-binding)
       (ser-push! (full-module-binding-module b))
       (ser-push! (full-module-binding-sym b))
       (ser-push! (full-module-binding-phase b))
       (ser-push! (full-module-binding-nominal-module b))
       (ser-push! (full-module-binding-nominal-phase b))
       (ser-push! (full-module-binding-nominal-sym b))
       (ser-push! (full-module-binding-nominal-require-phase b))
       (ser-push! (full-binding-free=id b))
       (if (full-module-binding-extra-inspector b)
           (ser-push! 'tag '#:inspector)
           (ser-push! #f))
       (ser-push! (full-module-binding-extra-nominal-bindings b))]
      [else
       (ser-push! simplified-b)])))

(struct simple-module-binding (module phase sym nominal-module)
  #:authentic
  #:transparent
  #:property prop:serialize
  (lambda (b ser-push! state)
    (ser-push! 'tag '#:simple-module-binding)
    (ser-push! (simple-module-binding-module b))
    (ser-push! (simple-module-binding-sym b))
    (ser-push! (simple-module-binding-phase b))
    (ser-push! (simple-module-binding-nominal-module b))))

(define (deserialize-full-module-binding module sym phase
                                         nominal-module
                                         nominal-phase
                                         nominal-sym
                                         nominal-require-phase
                                         free=id
                                         extra-inspector
                                         extra-nominal-bindings)
  (make-module-binding module phase sym
                       #:nominal-module nominal-module
                       #:nominal-phase nominal-phase
                       #:nominal-sym nominal-sym
                       #:nominal-require-phase nominal-require-phase
                       #:free=id free=id
                       #:extra-inspector extra-inspector
                       #:extra-nominal-bindings extra-nominal-bindings))

(define (deserialize-simple-module-binding module sym phase nominal-module)
  (simple-module-binding module phase sym nominal-module))

;; ----------------------------------------

(define (module-binding-module b)
  (if (simple-module-binding? b)
      (simple-module-binding-module b)
      (full-module-binding-module b)))

(define (module-binding-phase b)
  (if (simple-module-binding? b)
      (simple-module-binding-phase b)
      (full-module-binding-phase b)))

(define (module-binding-sym b)
  (if (simple-module-binding? b)
      (simple-module-binding-sym b)
      (full-module-binding-sym b)))

(define (module-binding-nominal-module b)
  (if (simple-module-binding? b)
      (simple-module-binding-nominal-module b)
      (full-module-binding-nominal-module b)))
       
(define (module-binding-nominal-phase b)
  (if (simple-module-binding? b)
      (simple-module-binding-phase b)
      (full-module-binding-nominal-phase b)))

(define (module-binding-nominal-sym b)
  (if (simple-module-binding? b)
      (simple-module-binding-sym b)
      (full-module-binding-nominal-sym b)))

(define (module-binding-nominal-require-phase b)
  (if (simple-module-binding? b)
      0
      (full-module-binding-nominal-require-phase b)))

(define (module-binding-extra-inspector b)
  (if (simple-module-binding? b)
      #f
      (full-module-binding-extra-inspector b)))

(define (module-binding-extra-nominal-bindings b)
  (if (simple-module-binding? b)
      null
      (full-module-binding-extra-nominal-bindings b)))
