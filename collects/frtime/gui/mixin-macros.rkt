(module mixin-macros frtime
  (require racket/class
           (for-syntax racket/base))
  
  
  (define-syntax events->callbacks
    (lambda (stx)
      (syntax-case stx (carries-args-for)
        [(_ field-name update-call)
         #'(lambda (super-class)
             (class ((events->callbacks field-name carries-args-for update-call)
                     super-class)
               (init (field-name (event-receiver)))
               (super-new (field-name (map-e list field-name)))))]
        [(_ field-name carries-args-for update-call)
         (let ([s-field-name (syntax field-name)])
           (with-syntax ([the-cell-name (string->symbol
                                         (format "~a-cell" (syntax-e s-field-name)))]
                         [getting-name (string->symbol
                                        (format "get-~a-e" (syntax-e s-field-name)))]
                         [renamed-update (string->symbol
                                        (format "renamed-~a" (syntax-e (syntax update-call))))])
             (syntax
              (lambda (super)
                (class super
                  (init (field-name (event-receiver)))
                  (super-new)
                  (inherit (renamed-update update-call))
                  (define the-cell-name field-name)
                  (for-each-e! the-cell-name
                               (lambda (evt) (renamed-update . evt))
                               this)
                  (define/public (getting-name) the-cell-name))))))])))
 

  
  ;; overridden method can have >1 form
  (define-syntax callbacks->args-evts
    (lambda (stx)
      (syntax-case stx ()
        [(_ ev-name method-name)
         (with-syntax ([name-e (string->symbol (format "~a-e" (syntax-e #'ev-name)))]
                       [g-name (string->symbol (format "get-~a" (syntax-e #'ev-name)))]
                       [processor (string->symbol (format "~a-event-processor" (syntax-e #'ev-name)))])
           #'(lambda (default-proc super-class)
               (class super-class
                 (init (processor default-proc))
                 (define name-e (event-receiver))
                 (define processed-events (processor name-e))
                 (super-new)
                 (define ft-last-evt #f)
                 ;what about when the super call returns an error?
                 (define/override method-name
                   (lambda args 
                     (when (or (< (length args) 2)
                               (and (not (eq? (cadr args) ft-last-evt))
                                    (set! ft-last-evt (cadr args))))
                       (send-event name-e args))
                     (super method-name . args)))
                 (define/public (g-name) processed-events))))])))
  

  
  (provide events->callbacks
           callbacks->args-evts))
