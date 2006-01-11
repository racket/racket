(module mixin-macros (lib "frtime.ss" "frtime")
  (require (lib "class.ss"))
  
  ;; consider taking out setter
  (define-syntax behavior->callbacks
    (lambda (stx)
      (syntax-case stx ()
        [(_ field-name update-call)
         (let ([s-field-name (syntax field-name)])
           (with-syntax ([the-cell-name (string->symbol
                                         (format "~a-cell" (syntax-e s-field-name)))]
                         [getting-name (string->symbol
                                        (format "get-~a-b" (syntax-e s-field-name)))])
             (syntax
              (lambda (default super)
                (class super
                  (init (field-name default))
                  
                  (super-new (field-name (value-now field-name)))
                  
                  (inherit update-call)
                  (define the-cell-name field-name)
                  
                  (for-each-e! (changes the-cell-name)
                               (lambda (evt) (update-call evt))
                               this)
                  
                  (define/public (getting-name) the-cell-name))))))])))
  
  (define-syntax events->callbacks
    (lambda (stx)
      (syntax-case stx ()
        [(_ field-name update-call)
         (let ([s-field-name (syntax field-name)])
           (with-syntax ([the-cell-name (string->symbol
                                         (format "~a-cell" (syntax-e s-field-name)))]
                         [getting-name (string->symbol
                                        (format "get-~a-e" (syntax-e s-field-name)))])
             (syntax
              (lambda (super)
                (class super
                  (init (field-name (event-receiver)))
                  (super-new)
                  (inherit update-call)
                  (define the-cell-name field-name)
                  
                  (for-each-e! the-cell-name
                               (lambda (evt) (update-call evt))
                               this)
                  
                  (define/public (getting-name) the-cell-name))))))])))
 
  ;; overridden method can have >1 form
  (define-syntax callbacks->args-evts
    (lambda (stx)
      (syntax-case stx ()
        [(_ ev-name method-name (arg ...))
         (with-syntax ([name-e (string->symbol (format "~a-e" (syntax-e #'ev-name)))]
                       [g-name (string->symbol (format "get-~a" (syntax-e #'ev-name)))]
                       [processor (string->symbol (format "~a-event-processor" (syntax-e #'ev-name)))])
           #'(lambda (super-class)
               (class super-class
                 (init (processor (lambda (x) x)))
                 (super-new)
                 (define name-e (event-receiver))
                 (define processed-events (processor name-e))
                 ;what about when the super call returns an error?
                 (define/override method-name
                   (lambda (arg ...)
                     (send-event name-e (list arg ...))
                     (super method-name arg ...)))
                 
                 (define/public (g-name) processed-events))))])))
  
  

  
  (provide behavior->callbacks
           events->callbacks
           callbacks->args-evts))
