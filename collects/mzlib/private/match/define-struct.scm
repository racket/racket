
(module define-struct mzscheme
  (require-for-syntax "struct-helper.scm")
  (provide define-struct*)
  
  (define-syntax (define-struct* stx)
    (syntax-case stx ()
      [(_ type [field-decl ...] decl ...)
       (let* ([field-decls (map (mk-parse-field-decl #'type) (syntax->list #'(field-decl ...)))]
              [decls (map parse-decl (syntax->list #'(decl ...)))]
              [info (create-info #'type decls field-decls)])
         (let ([init-field-k (length (info-init-fields info))]
               [auto-field-k (length (info-auto-fields info))])
           #`(begin
               #,(if (info-include-define-values? info)
                     #`(define-values #,(info-defined-names info)
                         (let-values 
                             ([(struct:x make-x x? x-ref x-set!)
                               (make-struct-type 'type
                                                 #,(info-super info)
                                                 #,init-field-k
                                                 #,auto-field-k
                                                 #,(info-auto-v info)
                                                 #,(info-props info)
                                                 #,(info-insp info)
                                                 #,(info-proc-spec info)
                                                 #,(info-imm-k-list info)
                                                 #,(info-guard info))])
                           (values struct:x
                                   make-x
                                   x?
                                   #,@(if (info-include-x-ref? info) #'(x-ref) #'())
                                   #,@(if (info-include-x-set!? info) #'(x-set!) #'())
                                   #,@(map (lambda (ref-field ref-posn)
                                             #`(make-struct-field-accessor 
                                                x-ref
                                                #,ref-posn
                                                '#,ref-field))
                                           (info-ref-fields info)
                                           (info-ref-posns info))
                                   #,@(map (lambda (mut-field mut-posn)
                                             #`(make-struct-field-mutator
                                                x-set!
                                                #,mut-posn
                                                '#,mut-field))
                                           (info-mut-fields info)
                                           (info-mut-posns info)))))
                     #'(begin))
               #,(if (info-include-replacers? info)
                     #`(define-struct-replacers type #,(info-name:constructor info)
                         #,(map field-decl-field (info-init-fields info))
                         #,(map field-decl-ref (info-init-fields info)))
                     #'(begin))
               #,(if (info-include-clone? info)
                     (with-syntax ([(field-ref ...) (map field-decl-ref (info-init-fields info))])
                       #`(define (#,(datum->syntax-object #'type (sym+ 'clone- #'type)) obj)
                           (let ([field-ref (field-ref obj)] ...)
                             (#,(info-name:constructor info) field-ref ...))))
                     #'(begin))
               #;#,(if (info-include-static-info? info)
                       #`(define-syntax type
                           (list-immutable
                            (quote-syntax #,(info-name:struct-record info))
                            (quote-syntax #,(info-name:constructor info))
                            (quote-syntax #,(info-name:predicate info))
                            (list-immutable
                             #,@(map (lambda (ref) #`(quote-syntax #,ref))
                                     (info-field-refs info)))
                            (list-immutable
                             #,@(map (lambda (mut) #`(quote-syntax #,mut))
                                     (info-field-muts info)))
                            ;; FIXME
                            #t))
                       #'(begin)))))]))
  
  (define-syntax (define-struct-replacers stx)
    (syntax-case stx ()
      [(_ type constructor (field ...) (accessor ...))
       (with-syntax 
           ([(replace ...)
             (map (lambda (f) (datum->syntax-object #'type (sym+ 'replace- #'type '- f)))
                  (syntax->list #'(field ...)))]
            [all-field-bindings #'([field (accessor obj)] ...)]
            [all-fields #'(field ...)])
         #'(begin (define (replace obj newval)
                    (let all-field-bindings
                      (let ([field newval])
                        (constructor . all-fields))))
                  ...))]))
  
  )
#|

(require struct)
(require (lib "pretty.ss"))
(print-struct #t)

(define-syntax go
  (syntax-rules ()
    [(_ form)
     (begin #;(pretty-print (syntax-object->datum (expand-once #'form)))
            form)]))
(go (define-struct* A 
      [x (y (immutable)) (z (auto)) (w (auto))]
      transparent (auto-value 'foo)))
(go (define-struct* B
      [q (r (immutable)) c]
      (procedure (lambda (self) (list (B-q self) (B-r self))))
      transparent clone replace))

(define a1 (make-A 'athens 'sparta))
(define b1 (make-B 'three 'fifty (lambda _ 'loch-ness)))
|#