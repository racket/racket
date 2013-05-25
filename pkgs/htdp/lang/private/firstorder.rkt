(module firstorder mzscheme
  (require racket/struct-info)
  (provide make-first-order
           make-first-order+struct
           first-order->higher-order
           fo+struct?
           fo?)
  
  (define (fo-either? x) (or (fo? x) (fo+struct? x)))
  (define (fo-either-proc-id x)
    (if (fo? x)
        (fo-proc-id x)
        (fo+struct-proc-id x)))
  
  (define-values (struct:fo make-first-order fo? fo-get fo-set!)
    (make-struct-type 'procedure #f 2 0 #f null (current-inspector) 0))
  
  (define-values (struct:fo+struct make-first-order+struct fo+struct? fo+struct-get fo+struct-set!)
    (let ()
      (define props (list (cons prop:struct-info (λ (s) (fo+struct-get s 2)))))
      (make-struct-type 'procedure #f 3 0 #f props (current-inspector) 0)))
  
  (define fo-proc-id (make-struct-field-accessor fo-get 1))
  (define fo+struct-proc-id (make-struct-field-accessor fo+struct-get 1))
  
  (define (first-order->higher-order id)
    (let ([v (syntax-local-value id (lambda () #f))])
      (if (or (fo-either? v)
              (and (set!-transformer? v)
                   (fo-either? (set!-transformer-procedure v))))
          (syntax-property
           (syntax-local-introduce 
            (fo-either-proc-id (if (fo-either? v) v (set!-transformer-procedure v))))
           'disappeared-use
           (syntax-local-introduce id))
          id))))
