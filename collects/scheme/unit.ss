
(module unit scheme/base
  (require mzlib/unit
           scheme/contract/base
           (for-syntax scheme/base
                       syntax/struct))
  (provide (except-out (all-from-out mzlib/unit)
                       struct struct/ctc)
           (rename-out [struct* struct]
                       [struct/ctc* struct/ctc]))

  ;; Replacement `struct' signature form:
  (define-signature-form (struct* stx)
    (syntax-case stx ()
      ((_ name (field ...) opt ...)
       (begin
         (unless (identifier? #'name)
           (raise-syntax-error #f
                               "expected an identifier to name the structure type"
                               stx
                               #'name))
         (for-each (lambda (field)
                     (unless (identifier? field)
                       (syntax-case field ()
                         [(id #:mutable)
                          (identifier? #'id)
                          'ok]
                         [_
                          (raise-syntax-error #f
                                              "bad field specification"
                                              stx
                                              field)])))
                   (syntax->list #'(field ...)))
         (let-values ([(no-ctr? mutable? no-stx? no-rt?)
                       (let loop ([opts (syntax->list #'(opt ...))]
                                  [no-ctr? #f]
                                  [mutable? #f]
                                  [no-stx? #f]
                                  [no-rt? #f])
                         (if (null? opts)
                             (values no-ctr? mutable? no-stx? no-rt?)
                             (let ([opt (car opts)])
                               (case (syntax-e opt)
                                 [(#:omit-constructor)
                                  (if no-ctr?
                                      (raise-syntax-error #f
                                                          "redundant option"
                                                          stx
                                                          opt)
                                      (loop (cdr opts) #t mutable? no-stx? no-rt?))]
                                 [(#:mutable)
                                  (if mutable?
                                      (raise-syntax-error #f
                                                          "redundant option"
                                                          stx
                                                          opt)
                                      (loop (cdr opts) no-ctr? #t no-stx? no-rt?))]
                                 [(#:omit-define-syntaxes)
                                  (if no-stx?
                                      (raise-syntax-error #f
                                                          "redundant option"
                                                          stx
                                                          opt)
                                      (loop (cdr opts) no-ctr? mutable? #t no-rt?))]
                                 [(#:omit-define-values)
                                  (if no-rt?
                                      (raise-syntax-error #f
                                                          "redundant option"
                                                          stx
                                                          opt)
                                      (loop (cdr opts) no-ctr? mutable? no-stx? #t))]
                                 [else
                                  (raise-syntax-error #f
                                                      (string-append
                                                       "expected a keyword to specify option: "
                                                       "#:mutable, #:omit-constructor, #:omit-define-syntaxes, or #:omit-define-values")
                                                      stx
                                                      opt)]))))])
           (cons
            #`(define-syntaxes (name)
                #,(build-struct-expand-info
                   #'name (syntax->list #'(field ...))
                   #f (not mutable?)
                   #f '(#f) '(#f)
                   #:omit-constructor? no-ctr?))
            (let ([names (build-struct-names #'name (syntax->list #'(field ...))
                                             #f (not mutable?))])
              (if no-ctr?
                  (cons (car names) (cddr names))
                  names))))))
      ((_ name fields opt ...)
       (raise-syntax-error #f
                           "bad syntax; expected a parenthesized sequence of fields"
                           stx
                           #'fields))
      ((_ name)
       (raise-syntax-error #f
                           "bad syntax; missing fields"
                           stx))
      ((_)
       (raise-syntax-error #f
                           "missing name and fields"
                           stx))))
  
  ;; Replacement struct/ctc form
  (define-signature-form (struct/ctc* stx)
    (syntax-case stx ()
      ((_ name ([field ctc] ...) opt ...)
       (begin
         (unless (identifier? #'name)
           (raise-syntax-error #f
                               "expected an identifier to name the structure type"
                               stx
                               #'name))
         (for-each (lambda (field)
                     (unless (identifier? field)
                       (syntax-case field ()
                         [(id #:mutable)
                          (identifier? #'id)
                          'ok]
                         [_
                          (raise-syntax-error #f
                                              "bad field specification"
                                              stx
                                              field)])))
                   (syntax->list #'(field ...)))
         (let-values ([(no-ctr? mutable? no-stx? no-rt?)
                       (let loop ([opts (syntax->list #'(opt ...))]
                                  [no-ctr? #f]
                                  [mutable? #f]
                                  [no-stx? #f]
                                  [no-rt? #f])
                         (if (null? opts)
                             (values no-ctr? mutable? no-stx? no-rt?)
                             (let ([opt (car opts)])
                               (case (syntax-e opt)
                                 [(#:omit-constructor)
                                  (if no-ctr?
                                      (raise-syntax-error #f
                                                          "redundant option"
                                                          stx
                                                          opt)
                                      (loop (cdr opts) #t mutable? no-stx? no-rt?))]
                                 [(#:mutable)
                                  (if mutable?
                                      (raise-syntax-error #f
                                                          "redundant option"
                                                          stx
                                                          opt)
                                      (loop (cdr opts) no-ctr? #t no-stx? no-rt?))]
                                 [(#:omit-define-syntaxes)
                                  (if no-stx?
                                      (raise-syntax-error #f
                                                          "redundant option"
                                                          stx
                                                          opt)
                                      (loop (cdr opts) no-ctr? mutable? #t no-rt?))]
                                 [(#:omit-define-values)
                                  (if no-rt?
                                      (raise-syntax-error #f
                                                          "redundant option"
                                                          stx
                                                          opt)
                                      (loop (cdr opts) no-ctr? mutable? no-stx? #t))]
                                 [else
                                  (raise-syntax-error #f
                                                      (string-append
                                                       "expected a keyword to specify option: "
                                                       "#:mutable, #:omit-constructor, #:omit-define-syntaxes, or #:omit-define-values")
                                                      stx
                                                      opt)]))))])
           (define (add-contracts l)
             (let* ([pred (caddr l)]
                    [ctor-ctc #`(-> ctc ... #,pred)]
                    [pred-ctc #'(-> any/c boolean?)]
                    [field-ctcs
                     (apply append
                            (map (λ (f c)
                                   (cons #`(-> #,pred #,c)
                                         (if (and (not mutable?)
                                                  (not (pair? (syntax-e f))))
                                             null
                                             #`(-> #,pred #,c void?))))
                                 (syntax->list #'(field ...))
                                 (syntax->list #'(ctc ...))))])
               (list* (car l)
                      (list (cadr l) ctor-ctc)
                      (list pred pred-ctc)
                      (map list (cdddr l) field-ctcs))))
           (cons
            #`(define-syntaxes (name)
                #,(build-struct-expand-info
                   #'name (syntax->list #'(field ...))
                   #f (not mutable?)
                   #f '(#f) '(#f)
                   #:omit-constructor? no-ctr?))
            (let* ([names (add-contracts
                           (build-struct-names #'name (syntax->list #'(field ...))
                                               #f (not mutable?)))]
                   [cpairs (cons 'contracted
                                 (if no-ctr? (cddr names) (cdr names)))])
              (list (car names) cpairs))))))
      ((_ name fields opt ...)
       (raise-syntax-error #f
                           "bad syntax; expected a parenthesized sequence of fields"
                           stx
                           #'fields))
      ((_ name)
       (raise-syntax-error #f
                           "bad syntax; missing fields"
                           stx))
      ((_)
       (raise-syntax-error #f
                           "missing name and fields"
                           stx)))))
