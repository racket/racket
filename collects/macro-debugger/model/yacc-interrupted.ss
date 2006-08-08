
(module yacc-interrupted mzscheme
  (require "deriv.ss"
           "yacc-ext.ss")
  (provide ! ? 
           production/I
           productions/I
           skipped-token-values
           %skipped
           %action)

  ;; Grammar macros for "interrupted parses"
  ;; Uses interrupted-wrap and error-wrap from deriv.ss
  
  (define-syntax !
    (lambda (stx)
      (raise-syntax-error #f "keyword ! used out of context" stx)))
  
  (define-syntax ?
    (lambda (stx)
      (raise-syntax-error #f "keyword ? used out of context" stx)))
  
  (define-syntax (productions/I stx)
    (syntax-case stx ()
      [(productions/I def ...)
       #'(begin (production/I def) ...)]))
  
  (define-for-syntax (partition-options/alternates forms)
    (let loop ([forms forms] [options null] [alts null])
      (if (pair? forms)
          (syntax-case (car forms) ()
            [(#:args . args)
             (loop (cdr forms) (cons (cons #:args #'args) options) alts)]
            [(#:skipped expr)
             (loop (cdr forms) (cons (cons #:skipped #'expr) options) alts)]
            [(#:no-interrupted)
             (loop (cdr forms) (cons (cons #:no-interrupted #t) options) alts)]
            [(#:no-wrap)
             (loop (cdr forms) (cons (cons #:no-wrap #t) options) alts)]
            [(#:no-wrap-error)
             (loop (cdr forms) (cons (cons #:no-wrap-error #t) options) alts)]
            [(kw . args)
             (keyword? (syntax-e #'kw))
             (raise-syntax-error #f "bad keyword" (car forms))]
            [(pattern action)
             (loop (cdr forms) options (cons (cons #'pattern #'action) alts))])
          (values options (reverse alts)))))

  (define-for-syntax (symbol+ . args)
    (define (norm x)
      (cond [(identifier? x) (norm (syntax-e x))]
            [(string? x) x]
            [(number? x) (number->string x)]
            [(symbol? x) (symbol->string x)]))
    (string->symbol (apply string-append (map norm args))))
  
  (define-for-syntax (I symbol)
    (syntax-local-introduce
     (syntax-local-get-shadower (datum->syntax-object #f symbol))))
  
  (define-for-syntax (elaborate-skipped-tail head tail action)
    (define new-tail
      (let loop ([parts tail])
        (syntax-case parts (? !)
          [() #'()]
          [(! . parts-rest) (loop #'((! #f) . parts-rest))]
          [((! expr) . parts-rest)
           (with-syntax ([NoError (I 'NoError)]
                         [parts-rest (loop #'parts-rest)])
             #'(NoError . parts-rest))]
          [((? NT) . parts-rest)
           (loop #'((? NT #f) . parts-rest))]
          [((? NT expr) . parts-rest)
           (loop #'(NT . parts-rest))]
          [(part0 . parts-rest)
           (identifier? #'part0)
           (with-syntax ([part0/Skipped (I (symbol+ #'part0 '/Skipped))]
                         [parts-rest (loop #'parts-rest)])
             #'(part0/Skipped . parts-rest))])))
    (with-syntax ([head head]
                  [new-tail new-tail])
      (cons #'(head . new-tail)
            action)))
  
  (define-for-syntax (elaborate-successful-alternate alt)
    (define pattern (car alt))
    (define action (cdr alt))
    (cons (let loop ([parts pattern])
            (syntax-case parts (? !)
              [() #'()]
              [(! . parts-rest)
               (loop #'((! #f) . parts-rest))]
              [((! expr) . parts-rest)
               (with-syntax ([NoError (I 'NoError)]
                             [parts-rest (loop #'parts-rest)])
                 #'(NoError . parts-rest))]
              [((? NT) . parts-rest)
               (loop #'((? NT #f) . parts-rest))]
              [((? NT expr) . parts-rest)
               (with-syntax ([parts-rest (loop #'parts-rest)])
                 #'(NT . parts-rest))]
              [(part0 . parts-rest)
               (identifier? #'part0)
               (with-syntax ([parts-rest (loop #'parts-rest)])
                 #'(part0 . parts-rest))]))
          action))
  
  (define-for-syntax (elaborate-interrupted-alternate alt wrap? wrap-error?)
    (define pattern (car alt))
    (define action (cdr alt))
    (let loop ([parts pattern] [position 1])
      (syntax-case parts (? !)
        [()
         ;; Can't be interrupted
         null]
        [(! . parts-rest)
         (loop #'((! #f) . parts-rest) position)]
        [((! expr) . parts-rest)
         (cons
          ;; Error occurs
          (with-syntax ([Error (I 'syntax-error #;Error)]
                        [action action]
                        [position-argument (I (symbol+ '$ position))])
            (elaborate-skipped-tail 
             #'Error
             #'parts-rest
             (if wrap-error?
                 #'(make-error-wrap position-argument expr action)
                 #'action)))
          ;; Error doesn't occur
          (with-syntax ([NoError (I 'NoError)])
            (loop #'(NoError . parts-rest) position)))]
        [((? NT) . parts-rest)
         (loop #'((? NT #f) . parts-rest) position)]
        [((? NT expr) . parts-rest)
         (cons 
          ;; NT is interrupted
          (with-syntax ([NT/I (I (symbol+ #'NT '/Interrupted))]
                        [action action])
            (elaborate-skipped-tail
             #'NT/I
             #'parts-rest 
             (if wrap?
                 #'(make-interrupted-wrap expr action)
                 #'action)))
          ;; NT is not interrupted
          (loop #'(NT . parts-rest) position))]
        [(part0 . parts-rest)
         (identifier? #'part0)
         (map (lambda (clause) (cons #`(part0 . #,(car clause)) (cdr clause)))
              (loop #'parts-rest (add1 position)))])))
  
  (define-syntax (production/I stx)
    (syntax-case stx ()
      [(production/I (name form ...))
       (let ()
         (define-values (options alternates)
           (partition-options/alternates (syntax->list #'(form ...))))
         (define successful-alternates
           (map elaborate-successful-alternate alternates))
         (define interrupted-alternates 
           (apply append 
                  (map (lambda (a)
                         (elaborate-interrupted-alternate a
                                                          (not (assq #:no-wrap options))
                                                          (not (assq #:no-wrap-error options))))
                       alternates)))
         (with-syntax ([((success-pattern . success-action) ...)
                        successful-alternates]
                       [((interrupted-pattern . interrupted-action) ...)
                        interrupted-alternates]
                       [skip-spec (assq #:skipped options)]
                       [args-spec (assq #:args options)]
                       [name/Skipped (I (symbol+ #'name '/Skipped))]
                       [name/Interrupted (I (symbol+ #'name '/Interrupted))]
                       [%action ((syntax-local-certifier) #'%action)])
           #`(begin
               (productions
                (name [success-pattern
                       (%action args-spec success-action)]
                      ...)
                (name/Skipped [() (%skipped args-spec skip-spec)]))
               #,(if (and (not (assq #:no-interrupted options))
                          (pair? interrupted-alternates))
                     #'(productions
                        (name/Interrupted [interrupted-pattern
                                           (%action args-spec interrupted-action)]
                                          ...))
                     #'(begin)))))]))
  
  (define-syntax (skipped-token-values stx)
    (syntax-case stx ()
      [(skipped-token-values)
       #'(begin)]
      [(skipped-token-values name . more)
       (identifier? #'name)
       (with-syntax ([name/Skipped (I (symbol+ #'name '/Skipped))])
         #'(begin (productions (name/Skipped [() #f]))
                  (skipped-token-values . more)))]
      [(skipped-token-values (name value) . more)
       (with-syntax ([name/Skipped (I (symbol+ #'name '/Skipped))])
         #'(begin (productions (name/Skipped [() value]))
                  (skipped-token-values . more)))]))
  
  (define-syntax (%skipped stx)
    (syntax-case stx ()
      [(%skipped args (#:skipped . expr))
       #'(%action args expr)]
      [(%skipped args #f)
       #'(%action args #f)]))
  
  (define-syntax (%action stx)
    (syntax-case stx ()
      [(elaborate-action (#:args . args) action)
       #'(lambda args action)]
      [(elaborate-action #f action)
       #'action]))
  
  )