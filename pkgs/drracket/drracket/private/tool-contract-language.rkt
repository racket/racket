#lang mzscheme

  (provide (rename -#%module-begin #%module-begin)
           (all-from-except mzscheme #%module-begin))
  
  (require mzlib/contract)
  (require-for-syntax mzlib/list)
  
  (define-syntax (-#%module-begin stx)
    
    (define-struct ctc-binding (var arg))
    (define-struct def-binding (var arg))
    
    (define (process-case case-stx)
      (syntax-case case-stx (define)
        [(define name expr)
         (identifier? (syntax name))
         (make-def-binding (syntax name) (syntax expr))]
        [(name type type-names strs ...)
         (and (identifier? (syntax name))
              (not (string? (syntax-object->datum (syntax type))))
              (andmap (λ (x) (string? (syntax-object->datum x))) (syntax->list (syntax (strs ...)))))
         (make-ctc-binding (syntax name) (syntax type))]
        [else (raise-syntax-error 'tool-contract-language.rkt "unknown case" stx case-stx)]))
    
    
    (syntax-case stx ()
      [(_ cases ...)
       (let* ([pcases (map process-case (syntax->list (syntax (cases ...))))]
              [def-cases (filter def-binding? pcases)]
              [ctc-cases (filter ctc-binding? pcases)])
         (with-syntax ([(ctc-name ...) (map ctc-binding-var ctc-cases)]
                       [(ctc ...) (map ctc-binding-arg ctc-cases)]
                       [(def-name ...) (map def-binding-var def-cases)]
                       [(def-exp ...) (map def-binding-arg def-cases)]
                       [wrap-tool-inputs (datum->syntax-object stx 'wrap-tool-inputs #'here)])
           (syntax/loc stx
             (#%module-begin
              (provide wrap-tool-inputs)
              (define-syntax wrap-tool-inputs
                (λ (in-stx)
                  (syntax-case in-stx ()
                    [(_ body tool-name)
                     (let ([f (λ (in-obj) 
                                (datum->syntax-object 
                                 in-stx
                                 (syntax-object->datum in-obj)
                                 in-obj))])
                       (with-syntax ([(in-type (... ...)) (map f (syntax->list (syntax (ctc ...))))]
                                     [(in-name (... ...)) (map f (syntax->list (syntax (ctc-name ...))))]
                                     [(in-def-name (... ...)) (map f (syntax->list (syntax (def-name ...))))]
                                     [(in-def-exp (... ...)) (map f (syntax->list (syntax (def-exp ...))))])
                         (syntax/loc in-stx
                           (let ([in-def-name in-def-exp] (... ...))
                             (let ([in-name (contract (let ([in-name in-type]) in-name)
                                                      in-name
                                                      'drscheme
                                                      tool-name
                                                      (quote-syntax in-name))] (... ...))
                               body)))))])))))))]
      [(_ (name type type-names strs ...) ...)
       (begin
         (for-each
          (λ (str-stx)
            (when (string? (syntax-object->datum str-stx))
              (raise-syntax-error 'tool-contract-language.rkt "expected type name specification"
                                  stx
                                  str-stx)))
          (syntax->list (syntax (type-names ...))))
         (for-each
          (λ (name)
            (unless (identifier? name)
              (raise-syntax-error 'tool-contract-language.rkt "expected identifier" stx name)))
          (syntax->list (syntax (name ...))))
         (for-each
          (λ (str)
            (unless (string? (syntax-object->datum str))
              (raise-syntax-error 'tool-contract-language.rkt "expected docs string" stx str)))
          (apply append (map syntax->list (syntax->list (syntax ((strs ...) ...)))))))]))
  
  (define-syntax (-#%module-begin2 stx)
    (syntax-case stx ()
      [(_ (name type type-names strs ...) ...)
       (and (andmap identifier? (syntax->list (syntax (name ...))))
            (andmap (λ (x) (not (string? (syntax-object->datum x))))
                    (syntax->list (syntax (type-names ...))))
            (andmap (λ (x) (string? (syntax-object->datum x)))
                    (apply append (map syntax->list (syntax->list (syntax ((strs ...) ...)))))))
       (with-syntax ([wrap-tool-inputs (datum->syntax-object stx 'wrap-tool-inputs #'here)])
         (syntax/loc stx
           (#%module-begin
            (provide wrap-tool-inputs)
            (define-syntax wrap-tool-inputs
              (λ (in-stx)
                (syntax-case in-stx ()
                  [(_ body tool-name)
                   (with-syntax ([(in-type (... ...))
                                  (map (λ (in-type-obj) 
                                         (datum->syntax-object 
                                          in-stx
                                          (syntax-object->datum in-type-obj)
                                          in-type-obj))
                                       (syntax->list (syntax (type ...))))]
                                 [(in-name (... ...))
                                  (map (λ (in-name-obj) 
                                         (datum->syntax-object 
                                          in-stx 
                                          (syntax-object->datum in-name-obj)
                                          in-name-obj))
                                       (syntax->list (syntax (name ...))))])
                     (syntax/loc in-stx
                       (let ([in-name (contract (let ([in-name in-type]) in-name)
                                                in-name
                                                'drscheme
                                                tool-name
                                                (quote-syntax in-name))] (... ...))
                         body)))]))))))]
      [(_ (name type type-names strs ...) ...)
       (begin
         (for-each
          (λ (str-stx)
            (when (string? (syntax-object->datum str-stx))
              (raise-syntax-error 'tool-contract-language.rkt "expected type name specification"
                                  stx
                                  str-stx)))
          (syntax->list (syntax (type-names ...))))
         (for-each
          (λ (name)
            (unless (identifier? name)
              (raise-syntax-error 'tool-contract-language.rkt "expected identifier" stx name)))
          (syntax->list (syntax (name ...))))
         (for-each
          (λ (str)
            (unless (string? (syntax-object->datum str))
              (raise-syntax-error 'tool-contract-language.rkt "expected docs string" stx str)))
          (apply append (map syntax->list (syntax->list (syntax ((strs ...) ...)))))))]))
