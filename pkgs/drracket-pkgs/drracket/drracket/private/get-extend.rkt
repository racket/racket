#lang racket/unit

(require racket/class
         drracket/private/drsig
         framework/private/logging-timer)

(import [prefix drracket:unit: drracket:unit^]
        [prefix drracket:frame: drracket:frame^]
        [prefix drracket:rep: drracket:rep^]
        [prefix drracket:debug: drracket:debug^]
        [prefix drracket:tracing: drracket:tracing^]
        [prefix drracket:module-language: drracket:module-language/int^]
        [prefix drracket:module-language-tools: drracket:module-language-tools^])
(export drracket:get/extend^)

(define re-extension-allowed? #f)
(define (allow-re-extension!) (set! re-extension-allowed? #t))
(define (disallow-re-extension!) (set! re-extension-allowed? #f))

(define make-extender
  (λ (get-base% name [final-mixin values])
    (define extend-name (string->symbol (format "extend-~a" name)))
    (let ([names-for-changes '()]
          [extensions '()]
          [built-yet? #f]
          [built #f]
          [verify
           (λ (f)
             (λ (%)
               (let ([new% (f %)])
                 (if (and (class? new%)
                          (subclass? new% %))
                     new%
                     (error extend-name "expected output of extension to create a subclass of its input, got: ~a"
                            new%)))))])
      (define (add-extender extension [before? #t] #:name-for-changes [name-for-changes #f])
        (cond
          [(and (symbol? name-for-changes) (member name-for-changes names-for-changes))
           (cond
             [re-extension-allowed?
              (set! extensions
                    (for/list ([e-extension (in-list extensions)]
                               [e-name (in-list names-for-changes)])
                      (if (equal? e-name name-for-changes) 
                          extension
                          e-extension)))
              (set! built-yet? #f)
              (set! built #f)]
             [else
              (error extend-name 
                     "attempted to use name ~s multiple times without first enabling re-extensions" 
                     name-for-changes)])]
          [else
           (when built-yet?
             (cond
               [re-extension-allowed? 
                (set! built-yet? #f)
                (set! built #f)]
               [else
                (error extend-name 
                       "cannot build a new extension of ~a after initialization"
                       name-for-changes)]))
           (set! extensions 
                 (if before?
                     (cons (verify extension) extensions)
                     (append extensions (list (verify extension)))))
           (set! names-for-changes
                 (if before?
                     (cons name-for-changes names-for-changes)
                     (append names-for-changes (list name-for-changes))))]))
      (define (get-built)
        (unless built-yet?
          (set! built-yet? #t)
          (set! built (final-mixin ((apply compose extensions) (get-base%)))))
        built)
      (values
       (procedure-rename add-extender extend-name)
       (procedure-rename get-built (string->symbol (format "get-~a" name)))))))

(define (get-base-tab%)
  (drracket:module-language:module-language-online-expand-tab-mixin
   (drracket:module-language-tools:tab-mixin
    (drracket:tracing:tab-mixin
     (drracket:debug:test-coverage-tab-mixin
      (drracket:debug:profile-tab-mixin
       drracket:unit:tab%))))))

(define-values (extend-tab get-tab) (make-extender get-base-tab% 'tab%))

(define (get-base-interactions-canvas%)
  drracket:unit:interactions-canvas%)

(define-values (extend-interactions-canvas get-interactions-canvas)
  (make-extender get-base-interactions-canvas% 'interactions-canvas%))

(define (get-base-definitions-canvas%)
  drracket:unit:definitions-canvas%)

(define-values (extend-definitions-canvas get-definitions-canvas)
  (make-extender get-base-definitions-canvas% 'definitions-canvas%))  

(define (get-base-unit-frame%) 
  (drracket:module-language-tools:frame-mixin
   (drracket:tracing:frame-mixin
    (drracket:debug:profile-unit-frame-mixin
     drracket:unit:frame%))))

(define-values (extend-unit-frame get-unit-frame)
  (make-extender get-base-unit-frame% 'drracket:unit:frame))

(define (get-base-interactions-text%)
  (drracket:module-language:module-language-online-expand-rep-mixin
   (drracket:module-language:module-language-big-defs/ints-interactions-text-mixin
    (drracket:debug:test-coverage-interactions-text-mixin
     drracket:rep:text%))))

(define-values (extend-interactions-text get-interactions-text)
  (make-extender get-base-interactions-text% 'interactions-text%))

(define (get-base-definitions-text%)
  (drracket:module-language:change-lang-host-mixin
   (drracket:module-language:module-language-online-expand-text-mixin
    (drracket:module-language-tools:definitions-text-mixin
     (drracket:module-language:module-language-big-defs/ints-definitions-text-mixin
      (drracket:debug:test-coverage-definitions-text-mixin
       (drracket:debug:profile-definitions-text-mixin
        (drracket:unit:get-definitions-text%))))))))

(define-values (extend-definitions-text get-definitions-text)
  (make-extender get-base-definitions-text%
                 'definitions-text%
                 (let ([add-on-paint-logging
                        (λ (%)
                          (class %
                            (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
                              (log-timeline 
                               (format "on-paint method of ~a area: ~a" (object-name this) (* (- right left) (- bottom top)))
                               (super on-paint before? dc left top right bottom dx dy draw-caret)))
                            (super-new)))])
                   add-on-paint-logging)))
