(module provide-transform '#%kernel
  (#%require "private/stxcase-scheme.rkt"
             "private/qqstx.rkt"
             "private/stx.rkt"
             "private/define-struct.rkt"
             "private/more-scheme.rkt"
             "private/small-scheme.rkt"
             "private/define.rkt")
  
  (#%provide expand-export 
             syntax-local-provide-certifier 
             make-provide-transformer prop:provide-transformer provide-transformer?
             ;; the export struct type:
             export struct:export make-export export?
             export-local-id export-out-sym export-orig-stx export-protect? export-mode)
  
  (define-struct* export (local-id out-sym mode protect? orig-stx)
    #:guard (lambda (i s mode protect? stx info)
              (unless (identifier? i)
                (raise-type-error 'make-export "identifier" i))
              (unless (symbol? s)
                (raise-type-error 'make-export "symbol" s))
              (unless (or (not mode)
                          (exact-integer? mode))
                (raise-type-error 'make-export "exact integer or #f" mode))
              (unless (syntax? stx)
                (raise-type-error 'make-export "syntax" stx))
              (values i s mode (and protect? #t) stx)))
    
  (define-values (prop:provide-transformer provide-transformer? provide-transformer-get-proc)
    (make-struct-type-property 'provide-transformer))
  
  (define-struct* pt (proc)
    #:property prop:provide-transformer (lambda (t) (pt-proc t)))
  
  (define (make-provide-transformer proc)
    (make-pt proc))
  
  ;; For backward compatibility:
  (define (syntax-local-provide-certifier)
    (case-lambda 
     [(v) v]
     [(v mark) v]))

  (define orig-insp (current-code-inspector))

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
