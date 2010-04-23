#lang scheme/unit

(require racket/class
         "drsig.ss")

(import [prefix drscheme:unit: drscheme:unit^]
        [prefix drscheme:frame: drscheme:frame^]
        [prefix drscheme:rep: drscheme:rep^]
        [prefix drscheme:debug: drscheme:debug^]
        [prefix drscheme:tracing: drscheme:tracing^]
        [prefix drscheme:module-language-tools: drscheme:module-language-tools^])
(export drscheme:get/extend^)

(define make-extender
  (λ (get-base% name)
    (let ([extensions (λ (x) x)]
          [built-yet? #f]
          [built #f]
          [verify
           (λ (f)
             (λ (%)
               (let ([new% (f %)])
                 (if (and (class? new%)
                          (subclass? new% %))
                     new%
                     (error 'extend-% "expected output of extension to create a subclass of its input, got: ~a"
                            new%)))))])
      (values
       (letrec ([add-extender
                 (case-lambda
                   [(extension) (add-extender extension #t)]
                   [(extension before?)
                    (when built-yet?
                      (error 'extender "cannot build a new extension of ~a after initialization"
                             name))
                    (set! extensions 
                          (if before?
                              (compose (verify extension) extensions)
                              (compose extensions (verify extension))))])])
         add-extender)
       (λ ()
         (unless built-yet?
           (set! built-yet? #t)
           (set! built (extensions (get-base%))))
         built)))))

(define (get-base-tab%)
  (drscheme:module-language-tools:tab-mixin
   (drscheme:tracing:tab-mixin
    (drscheme:debug:test-coverage-tab-mixin
     (drscheme:debug:profile-tab-mixin
      drscheme:unit:tab%)))))

(define-values (extend-tab get-tab) (make-extender get-base-tab% 'tab%))

(define (get-base-interactions-canvas%)
  drscheme:unit:interactions-canvas%)

(define-values (extend-interactions-canvas get-interactions-canvas)
  (make-extender get-base-interactions-canvas% 'interactions-canvas%))

(define (get-base-definitions-canvas%)
  drscheme:unit:definitions-canvas%)

(define-values (extend-definitions-canvas get-definitions-canvas)
  (make-extender get-base-definitions-canvas% 'definitions-canvas%))  

(define (get-base-unit-frame%) 
  (drscheme:module-language-tools:frame-mixin
   (drscheme:tracing:frame-mixin
    (drscheme:debug:profile-unit-frame-mixin
     drscheme:unit:frame%))))

(define-values (extend-unit-frame get-unit-frame)
  (make-extender get-base-unit-frame% 'drscheme:unit:frame))

(define (get-base-interactions-text%)
  (drscheme:debug:test-coverage-interactions-text-mixin
   drscheme:rep:text%))

(define-values (extend-interactions-text get-interactions-text)
  (make-extender get-base-interactions-text% 'interactions-text%))

(define (get-base-definitions-text%)
  (drscheme:module-language-tools:definitions-text-mixin
   (drscheme:debug:test-coverage-definitions-text-mixin
    (drscheme:debug:profile-definitions-text-mixin
     (drscheme:unit:get-definitions-text%)))))

(define-values (extend-definitions-text get-definitions-text)
  (make-extender get-base-definitions-text% 'definitions-text%))
