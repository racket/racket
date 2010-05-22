#lang scheme
(require
 (for-syntax scheme/match
             scheme/struct-info
             "define.ss"
             "function.ss"
             "syntax.ss"))

(define-syntax-rule (match? e p ...)
  (match e [p #t] ... [_ #f]))

(define-syntax (define-struct-pattern stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ pattern-name struct-name)
       (block

        (define pattern-id #'pattern-name)
        (define struct-id #'struct-name)

        (unless (identifier? pattern-id)
          (syntax-error pattern-id "expected an identifier"))

        (unless (identifier? struct-id)
          (syntax-error struct-id "expected an identifier"))

        (define struct-info (syntax-local-value struct-id))

        (unless (struct-info? struct-info)
          (syntax-error struct-id "expected a struct name"))

        (match (extract-struct-info struct-info)
          [(list type-id
                 constructor-id
                 predicate-id
                 accessor-ids
                 mutator-ids
                 super-id)
           (with-syntax ([make constructor-id]
                         [(p ...) (generate-temporaries accessor-ids)])
             (syntax/loc stx
               (define-match-expander pattern-name
                 (syntax-rules ()
                   [(_ p ...) (struct struct-name [p ...])])
                 (redirect-transformer #'make))))]))])))

(define-for-syntax (get-struct-info id)
  (block

   (define (fail)
     (syntax-error id "expected a structure name"))

   (define value
     (syntax-local-value id fail))

   (unless (struct-info? value) (fail))

   (extract-struct-info value)))

(define-for-syntax (struct-match-expander stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ s f ...)
       (match (get-struct-info #'s)
         [(list _
                _
                (? identifier? pred)
                (list-rest (? identifier? rev-gets) ... (or (list) (list #f)))
                _
                _)
          (let* ([n-patterns (length (syntax-list f ...))]
                 [n-fields (length rev-gets)])
            (unless (= n-patterns n-fields)
              (syntax-error #'s
                            "got ~a patterns for ~a fields of ~a"
                            n-patterns n-fields (syntax-e #'s))))
          (with-syntax ([pred? pred]
                        [(get ...) (reverse rev-gets)])
            (syntax/loc stx
              (and (? pred?) (app get f) ...)))]
         [_
          (syntax-error
           #'s
           "expected a structure name with predicate and ~a fields; got ~a"
           (length (syntax-list f ...))
           (syntax-e #'s))])])))

(define-for-syntax (struct-make-expander stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ s f ...)
       (match (get-struct-info #'s)
         [(list _
                (? identifier? make)
                _
                rev-gets
                _
                _)
          (match rev-gets
            [(list (? identifier?) ...)
             (let* ([n-fields (length rev-gets)]
                    [n-exprs (length (syntax-list f ...))])
               (unless (= n-exprs n-fields)
                 (syntax-error
                  #'s
                  "got ~a arguments for ~a fields in structure ~a"
                  n-exprs n-fields (syntax-e #'s))))]
            [_ (void)])
          (with-syntax ([mk make])
            (syntax/loc stx
              (mk f ...)))]
         [_
          (syntax-error
           #'s
           "expected a structure name with constructor; got ~a"
           (syntax-e #'s))])])))

(define-match-expander $
  ;; define-match-expander is STUPIDLY non-uniform about variable expressions
  (identity struct-match-expander)
  (identity struct-make-expander))

(define-match-expander as
  (syntax-rules ()
    [(as ([x e] ...) p ...) (and (app (lambda (y) e) x) ... p ...)]))

(provide match? define-struct-pattern $ as)
