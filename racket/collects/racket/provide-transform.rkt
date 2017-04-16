(module provide-transform '#%kernel
  (#%require "private/stxcase-scheme.rkt"
             "private/stx.rkt"
             "private/define-struct.rkt"
             "private/small-scheme.rkt"
             "private/more-scheme.rkt"
             "private/define.rkt"
             (for-syntax '#%kernel
                         "private/stxcase-scheme.rkt"
                         "private/stx.rkt"
                         "private/define-struct.rkt"
                         "private/small-scheme.rkt"
                         "private/define.rkt"))
  
  (#%provide expand-export pre-expand-export 
             syntax-local-provide-certifier 
             make-provide-transformer prop:provide-transformer provide-transformer?
             make-provide-pre-transformer prop:provide-pre-transformer provide-pre-transformer?
             with-provide-disappeared-uses provide-record-disappeared-use
             ;; the export struct type:
             export struct:export make-export export?
             export-local-id export-out-sym export-orig-stx export-protect? export-mode)
  
  (define-struct* export (local-id out-sym mode protect? orig-stx)
    #:guard (lambda (i s mode protect? stx info)
              (unless (identifier? i)
                (raise-argument-error 'make-export "identifier?" i))
              (unless (symbol? s)
                (raise-argument-error 'make-export "symbol?" s))
              (unless (or (not mode)
                          (exact-integer? mode))
                (raise-argument-error 'make-export "(or/c exact-integer? #f)" mode))
              (unless (syntax? stx)
                (raise-argument-error 'make-export "syntax?" stx))
              (values i s mode (and protect? #t) stx)))
    
  (define-values (prop:provide-pre-transformer provide-pre-transformer? provide-pre-transformer-get-proc)
    (make-struct-type-property 'provide-pre-transformer))
  
  (define-values (prop:provide-transformer provide-transformer? provide-transformer-get-proc)
    (make-struct-type-property 'provide-transformer))
  
  (define-struct* pt (proc)
    #:property prop:provide-transformer (lambda (t) (pt-proc t)))
  (define-struct* p+t (pre-proc proc)
    #:property prop:provide-transformer (lambda (t) (p+t-proc t))
    #:property prop:provide-pre-transformer (lambda (t) (p+t-pre-proc t)))
  
  (define make-provide-transformer
    (case-lambda
     [(proc)
      (make-pt proc)]
     [(proc pre-proc)
      (make-p+t pre-proc proc)]))
  
  (define-struct* ppt (proc)
    #:property prop:provide-pre-transformer (lambda (t) (ppt-proc t)))
  
  (define (make-provide-pre-transformer proc)
    (make-ppt proc))
  
  ;; For backward compatibility:
  (define (syntax-local-provide-certifier)
    (case-lambda 
     [(v) v]
     [(v mark) v]))

  (define orig-insp (variable-reference->module-declaration-inspector
                     (#%variable-reference)))

  (define provide-disappeared-uses (make-parameter #f))
  (define-syntax (with-provide-disappeared-uses stx)
    (syntax-case stx ()
      ([_ expr]
       #'(let-values ([(v ids) (parameterize ([provide-disappeared-uses '()])
                                 (let ([v expr])
                                   (values v (provide-disappeared-uses))))])
           (syntax-property v 'disappeared-use
             (append ids (or (syntax-property v 'disappeared-use) null)))))))

  (define (provide-record-disappeared-use id)
    (let ([old (provide-disappeared-uses)])
      (when old
        (provide-disappeared-uses (cons id old)))))

  (define (pre-expand-export stx modes)
    (if (identifier? stx)
        stx
        (let ([disarmed-stx (syntax-disarm stx orig-insp)])
          (syntax-case disarmed-stx ()
            [(id . rest)
             (identifier? #'id)
             (let ([t (syntax-local-value #'id (lambda () #f))])
               (if (provide-pre-transformer? t)
                   (let ([v (((provide-pre-transformer-get-proc t) t) disarmed-stx modes)])
                     (provide-record-disappeared-use (syntax-local-introduce #'id))
                     (unless (syntax? v)
                       (raise-syntax-error
                        #f
                        "result from provide pre-transformer is not a syntax object"
                        stx))
                     (syntax-property v
                                      'disappeared-use
                                      (cons (syntax-local-introduce #'id)
                                            (or (syntax-property v 'disappeared-use) null))))
                   stx))]
            [_ stx]))))

  ;; expand-export : stx -> (listof export)
  (define (expand-export stx modes)
    (if (identifier? stx)
        (apply
         append
         (map (lambda (mode)
                (list (make-export stx (syntax-e stx) mode #f stx)))
              (if (null? modes)
                  '(0)
                  modes)))
        (let ([disarmed-stx (syntax-disarm stx orig-insp)])
          (syntax-case disarmed-stx ()
            [(id . rest)
             (identifier? #'id)
             (let ([t (syntax-local-value #'id (lambda () #f))])
               (if (provide-transformer? t)
                   (let ([v (((provide-transformer-get-proc t) t) disarmed-stx modes)])
                     (provide-record-disappeared-use (syntax-local-introduce #'id))
                     (unless (and (list? v)
                                  (andmap export? v))
                       (raise-syntax-error
                        #f
                        "result from provide transformer is not a list of exports"
                        stx))
                     v)
                   (raise-syntax-error
                    #f
                    "not a provide sub-form"
                    stx)))]
            [_
             (raise-syntax-error
              #f
              "bad syntax for provide sub-form"
              stx)])))))
