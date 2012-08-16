(module aux-mixin-macros frtime
  (require "mixin-macros.rkt")
  (require racket/class)
  (require (for-syntax racket/base))

  
  ;; consider taking out setter
  ; currently, get-<field-name> will return an event stream
  (define-syntax behavior->callbacks
    (lambda (stx)
      (syntax-case stx ()
        [(_ field-name update-call)
         (let ([s-field-name (syntax field-name)])
           (with-syntax ([the-cell-name (string->symbol
                                         (format "~a-cell" (syntax-e s-field-name)))]
                         [init-beh-val (string->symbol
                                        (format "value-now-~a-b" (syntax-e s-field-name)))])
             (syntax
              (lambda (default super)
                (class ((events->callbacks field-name update-call)
                        (class super
                          (init init-beh-val)
                          (super-new (field-name init-beh-val))))
                  (init (field-name default))
                  (super-new (field-name (changes (default . until . field-name)))
                             (init-beh-val (value-now (default . until . field-name))))
                  )))))]
         )))
  
  
  (define-syntax (embed-processor stx)
    (syntax-case stx ()
      [(_ processed-name processor getter ...)
       (with-syntax ([processed-getter (string->symbol
                                        (format "get-~a" (syntax-e (syntax processed-name))))])
         #'(lambda (super-class)
             (class super-class
               (super-new)
               (inherit getter ...)
               (define processed-name (processor (getter) ...))
               (define/public (processed-getter) processed-name))))]))
       
   ; merges event streams created by callbacks->args-evts
  (define-syntax (mixin-merge-e stx)
    (syntax-case stx ()
      [(_ result-name get-name ...)
       #'(embed-processor result-name 
                          (lambda args (apply merge-e args)) 
                          get-name ...)]))
  
  ; given a name for a behavior, an init-field name, and a getter method,
  ; produces get-<behavior-name> which is the hold of calling the getter method
  ; with the initial value being init-field-name
  (define-syntax (mixin-hold stx)
    (syntax-case stx ()
      [(_ b-name get-init get-event-stream)
       #'(embed-processor 
          b-name 
          (lambda (es) (hold es (send this get-init))) 
          get-event-stream)]))
  
            
  
  ; batch application of behavior->callbacks
  (define-syntax add-signal-controls
    (syntax-rules ()
      [(_ src (field-name0 update-call0 default-val0) clause ...)
       ((behavior->callbacks field-name0 update-call0)
        default-val0
        (add-signal-controls src clause ...))]
      [(_ src)
       src]))
  
  (provide (all-defined-out)))
