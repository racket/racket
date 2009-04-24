(module parser-sigs scheme
  
  (require (only-in mzlib/etc opt-lambda))    ; Required for expansion
  (require parser-tools/lex
           mzlib/string)
  
  (provide (all-defined-out))
  
  (define-signature-form (terminals stx)
    (syntax-case stx ()
      [(_ group (elt ...))
       (and (identifier? #'group)
            (andmap identifier? (syntax->list #'(elt ...))))
       (syntax->list #`(elt ...
                        #,@(map (lambda (e) 
                                  (datum->syntax e
                                                 (string->symbol 
                                                  (format "token-~a" (syntax-e e)))))
                                (syntax->list #'(elt ...)))))]))
  
  (define-signature-form (recurs stx)
    (syntax-case stx ()
      [(_ id ...)
       (andmap identifier? (syntax->list #'(id ...)))
       (syntax->list #`(id ...
                        #,@(map (lambda (e) #`(define-syntaxes 
                                                (#,(datum->syntax e (string->symbol (format "~a@" (syntax-e e)))))
                                                (values (syntax-id-rules () [_ (opt-lambda (x [s (list 0 1 0 1)] [o 1]) (#,e x s o))]))))
                                (syntax->list #'(id ...)))))]))
  
  (define-signature language-dictionary^ (misspelled misscap missclass))
    
  (define-signature combinator-parser-forms^ 
    (terminal choice seq repeat repeat-greedy
     (define-syntaxes (define-simple-terminals)
       (values 
        (lambda (stx)
          (syntax-case stx ()
            ((_ group elts)
             (let ([name-string-thunks 
                    (let loop ([elt-list (syntax elts)])
                      (syntax-case elt-list (lambda)
                        [() null]
                        [(id . rest)
                         (identifier? (syntax id))
                         (cons (list (syntax id)
                                     (syntax (symbol->string (quote id)))
                                     `(lambda (x . args) x))
                               (loop (syntax rest)))]
                        [((id name) . rest)
                         (and (identifier? (syntax id)) (string? (syntax-e (syntax name))))
                         (cons (list (syntax id)
                                     (syntax name)
                                     `(lambda (x . args) x))
                               (loop (syntax rest)))]
                        [((id thunk) . rest)
                         (and (identifier? (syntax id)) (identifier? (syntax thunk)))
                         (cons (list (syntax id)
                                     (syntax (symbol->string (quote id)))
                                     (syntax thunk))
                               (loop (syntax rest)))]
                        [((id (lambda x body ...)) . rest)
                         (identifier? (syntax id))
                         (cons (list (syntax id)
                                     (syntax (symbol->string (quote id)))
                                     (syntax (lambda x body ...)))
                               (loop (syntax rest)))]
                        [((id name thunk) . rest)
                         (and (identifier? (syntax id)) (string? (syntax-e (syntax name))))
                         (cons (list (syntax id)
                                     (syntax name)
                                     (syntax thunk))
                               (loop (syntax rest)))]))])
               (with-syntax ([(id ...) (map car name-string-thunks)]
                             [(name ...) (map cadr name-string-thunks)]
                             [(thunk ...) (map caddr name-string-thunks)])
                 (syntax
                  (begin
                    (define-empty-tokens group (id ...))
                    (define id
                      (terminal 
                       (lambda (token) (eq? (token-name token) (quote id)))
                       thunk
                       name)) ...)))))))))
     
     (define-syntaxes (define-terminals)
       (values 
        (lambda (stx)
          (syntax-case stx ()
            [(_ group elts)
             (identifier? (syntax group))
             (let ([name-string-thunks 
                    (let loop ([elt-list (syntax elts)])
                      (syntax-case elt-list (lambda)
                        [() null]
                        [((id (lambda (arg1 ...) body ...)) . rest)
                         (identifier? (syntax id))
                         (cons (list (syntax id)
                                     (syntax (symbol->string (quote id)))
                                     (syntax (lambda (arg1 ...) body ...)))
                               (loop (syntax rest)))]
                        [((id thunk) . rest)
                         (and (identifier? (syntax id)) (identifier? (syntax thunk)))
                         (cons (list (syntax id)
                                     (syntax (symbol->string (quote id)))
                                     (syntax thunk))
                               (loop (syntax rest)))]
                        [((id name thunk) . rest)
                         (cons (list (syntax id)
                                     (syntax name)
                                     (syntax thunk))
                               (loop (syntax rest)))]))])
               (with-syntax ([(id ...) (map car name-string-thunks)]
                             [(name ...) (map cadr name-string-thunks)]
                             [(thunk ...) (map caddr name-string-thunks)])
                 (syntax
                  (begin
                    (define-tokens group (id ...))
                    (define id
                      (terminal 
                       (lambda (token) (eq? (token-name token) (quote id)))
                       (lambda (x . args) 
                         (if (null? args)
                             (thunk (token-value x))
                             (thunk (token-value x) (car args) (cadr args))))
                       name
                       (lambda (token) 0)
                       (lambda (token) #f))) ...))))]))))
        
     (define-syntaxes (sequence choose ^)
       (let ([insert-name
              (lambda (stx name)
                (let loop ([term stx]
                           [pos 0]
                           [id-pos 0]
                           [terms null])
                  (syntax-case* term (sequence choose ^) 
                    (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
                    [((sequence a b) . rest)
                     (loop (syntax rest) (add1 pos) id-pos
                           (cons (quasisyntax (sequence a b #,name)) terms))]
                    [((choose a) . rest)
                     (loop (syntax rest) (add1 pos) id-pos
                           (cons (quasisyntax (choose a #,name)) terms))]
                    [((^ a) . rest)
                     (loop (syntax (a . rest))
                           pos (add1 pos) terms)]
                    [(a . rest)
                     (loop (syntax rest) (add1 pos) id-pos (cons (syntax a) terms))]
                    [() (list (reverse terms) id-pos)])))])
         (values
          (lambda (stx)
            (syntax-case stx (^)
              [(_ (term ...) proc) 
               (syntax
                (seq (list term ...) proc (symbol->string (gensym 'seq))))]
              [(_ terms proc name)
               (let ([new-terms (insert-name (syntax terms) (syntax name))])
                 (with-syntax (((term ...) (car new-terms))
                               (id-pos (cadr new-terms)))
                   (syntax (seq (list term ...) proc name id-pos))))]))
          (lambda (stx)
            (syntax-case stx ()
              [(_ (term ...))
               (syntax
                (choice (list term ...) (symbol->string (gensym 'choice))))]
              [(_ terms name)
               (with-syntax (((term ...) [car (insert-name (syntax terms) (syntax name))]))
                 (syntax
                  (choice (list term ...) name)))]))
          (syntax-rules ()
            [(_ f) f]))))
     
     (define-syntaxes (eta)
       (values (syntax-rules ()
                 [(_ f)
                  (opt-lambda (x [s (list 0 1 0 1)] [o 1]) (f x s o))])))
     ))
    
  (define-signature parser^ (parser))
  (define-signature out^ ((struct err (msg src))))
  
  (define-signature language-format-parameters^ (class-type input->output-name))
  
  (define-signature error-format-parameters^ 
    (src? input-type show-options max-depth max-choice-depth))
  
  (define-signature ranking-parameters^ 
    (rank-misspell rank-caps rank-class rank-wrong rank-end rank-choice rank-repeat))
  
  (define-signature updating-rank^
    (blamed-terminal failed-last-parse))
 
  (define-signature error^ (fail-type->message))
  
  (define-signature combinator-parser^ extends combinator-parser-forms^ (parser))
  (define-signature err^ (err? err-msg err-src))
    
  )
