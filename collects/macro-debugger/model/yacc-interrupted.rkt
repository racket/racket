#lang racket/base
(require (for-syntax racket/base
                     racket/syntax)
         "yacc-ext.rkt")
(provide ! ? !!
         define-production-splitter
         skipped-token-values
         %skipped
         %action)

;; Grammar macros for "interrupted parses"

(define-syntax !
  (lambda (stx)
    (raise-syntax-error #f "keyword ! used out of context" stx)))

(define-syntax !!
  (lambda (stx)
    (raise-syntax-error #f "keyword !! used out of context" stx)))

(define-syntax ?
  (lambda (stx)
    (raise-syntax-error #f "keyword ? used out of context" stx)))

(define-syntax define-production-splitter
  (syntax-rules ()
    [(define-production-splitter name ok intW)
     (define-syntax name
       (make-production-splitter #'ok #'intW))]))

(define-for-syntax (partition-options/alternates forms)
  (let loop ([forms forms] [options null] [alts null])
    (if (pair? forms)
        (syntax-case (car forms) ()
          [(#:args . args)
           (loop (cdr forms) (cons (cons '#:args #'args) options) alts)]
          [(#:skipped expr)
           (loop (cdr forms) (cons (cons '#:skipped #'expr) options) alts)]
          [(#:wrap)
           (loop (cdr forms) (cons (cons '#:wrap #t) options) alts)]
          [(#:no-wrap)
           (loop (cdr forms) (cons (cons '#:no-wrap #t) options) alts)]
          [(kw . args)
           (keyword? (syntax-e #'kw))
           (raise-syntax-error 'split "bad keyword" (car forms))]
          [(pattern action)
           (loop (cdr forms) options (cons (cons #'pattern #'action) alts))]
          [other
           (raise-syntax-error 'split "bad grammar option or alternate" #'other)])
        (values options (reverse alts)))))

(define-for-syntax (I symbol)
  (syntax-local-introduce
   (syntax-local-get-shadower (datum->syntax #f symbol))))

(define-for-syntax ($name n)
  (I (format-symbol "$~a" n)))

(define-for-syntax (interrupted-name id)
  (I (format-symbol "~a/Interrupted" (syntax-e id))))

(define-for-syntax (skipped-name id)
  (I (format-symbol "~a/Skipped" (syntax-e id))))

(define-for-syntax (elaborate-skipped-tail head tail position args mk-action)
  (define-values (new-tail new-arguments)
    (let loop ([parts tail] [position position] [rtail null] [arguments null])
      (syntax-case parts (? ! !!)
        [()
         (values (reverse rtail) (reverse arguments))]
        [(! . parts-rest)
         (loop #'parts-rest position rtail (cons #'#f arguments))]
        [(!! . parts-rest)
         (raise-syntax-error 'split
                             "cannot have !! after potential error"
                             #'!!)]
        [((? NT) . parts-rest)
         (loop #'(NT . parts-rest) position rtail arguments)]
        [(NT . parts-rest)
         (identifier? #'NT)
         (loop #'parts-rest
               (add1 position)
               (cons (skipped-name #'NT) rtail)
               (cons ($name position) arguments))])))
  (define arguments (append (reverse args) new-arguments))
  (cons #`(#,head . #,new-tail)
        (mk-action arguments)))

(define-for-syntax ((make-elaborate-successful-alternate wrap? okW) alt)
  (define pattern (car alt))
  (define action-function (cdr alt))
  (define-values (new-patterns arguments)
    (let loop ([parts pattern] [rpattern null] [position 1] [args null])
      (syntax-case parts (? ! !!)
        [() (values (list (reverse rpattern)) (reverse args))]
        [(! . parts-rest)
         (loop #'parts-rest rpattern position (cons #'#f args))]
        [(!!)
         (values null null)]
        [((? NT) . parts-rest)
         (loop (cons #'NT #'parts-rest) rpattern position args)]
        [(NT . parts-rest)
         (identifier? #'NT)
         (loop #'parts-rest (cons #'NT rpattern)
               (add1 position) (cons ($name position) args))])))
  (map (lambda (new-pattern)
         (cons (datum->syntax #f new-pattern pattern)
               #`(#,action-function #,(if wrap? okW #'values) #,@arguments)))
       new-patterns))

(define-for-syntax ((make-elaborate-interrupted-alternate wrap? intW) alt)
  (define pattern (car alt))
  (define action-function (cdr alt))
  (define (int-action args)
    (let ([wrapf (if wrap? #`(lambda (x) (#,intW x)) #'values)])
      #`(#,action-function #,wrapf #,@args)))
  (let loop ([parts pattern] [position 1] [args null])
    (syntax-case parts (? ! !!)
      [()
       ;; Can't be interrupted
       null]
      [(! . parts-rest)
       (cons
        ;; Error occurs
        (elaborate-skipped-tail (I 'syntax-error)
                                #'parts-rest
                                (add1 position)
                                (cons ($name position) args)
                                int-action)
        ;; Error doesn't occur
        (loop #'parts-rest position (cons #'#f args)))]
      [(!!)
       (cons
        (elaborate-skipped-tail (I 'syntax-error)
                                #'()
                                (add1 position)
                                (cons ($name position) args)
                                int-action)
        null)]
      [((? NT) . parts-rest)
       (cons 
        ;; NT is interrupted
        (elaborate-skipped-tail (interrupted-name #'NT)
                                #'parts-rest
                                (add1 position)
                                (cons ($name position) args)
                                int-action)
        ;; NT is not interrupted
        (loop #'(NT . parts-rest) position args))]
      [(part0 . parts-rest)
       (identifier? #'part0)
       (map (lambda (clause) (cons #`(part0 . #,(car clause)) (cdr clause)))
            (loop #'parts-rest (add1 position) (cons ($name position) args)))])))

(define-for-syntax (generate-action-name nt pos)
  (syntax-local-get-shadower
   (format-id #f "action-for-~a/~a" (syntax-e nt) pos)))

(define-for-syntax ((make-rewrite-alt+def nt args-spec) alt pos)
  (define pattern (car alt))
  (define action (cdr alt))
  (define-values (var-indexes non-var-indexes)
    (let loop ([pattern pattern] [n 1] [vars null] [nonvars null])
      (syntax-case pattern ()
        [(first . more)
         (syntax-case #'first (! ? !!)
           [!
            (loop #'more (add1 n) (cons n vars) nonvars)]
           [(! . _)
            (raise-syntax-error 'split
                                "misuse of ! grammar form"
                                pattern #'first)]
           [!!
            (when (pair? (syntax-e #'more))
              (raise-syntax-error 'split
                                  "nothing may follow !!"
                                  pattern))
            (loop #'more (add1 n) (cons n vars) nonvars)]
           [(!! . _)
            (raise-syntax-error 'split
                                "misuse of !! grammar form"
                                pattern #'first)]
           [(? NT)
            (identifier? #'NT)
            (loop #'more (add1 n) (cons n vars) nonvars)]
           [(? . _)
            (raise-syntax-error 'split
                                "misuse of ? grammar form"
                                pattern #'first)]
           [NT
            (identifier? #'NT)
            (loop #'more (add1 n) (cons n vars) nonvars)]
           [other
            (raise-syntax-error 'rewrite-pattern
                                "invalid grammar pattern"
                                pattern #'first)])]
        [()
         (values (reverse vars) (reverse nonvars))])))
  (define variables (map $name var-indexes))
  (define non-var-names (map $name non-var-indexes))
  (define action-function (generate-action-name nt pos))
  (cons (cons pattern action-function)
        (with-syntax ([(var ...) variables]
                      [(nonvar ...) non-var-names]
                      [action-function action-function]
                      [action action])
          #`(define (action-function wrap var ...)
              (let-syntax ([nonvar invalid-$name-use] ...)
                #,(if args-spec
                      #`(lambda #,args-spec (wrap action))
                      #`(wrap action)))))))

(define-for-syntax (invalid-$name-use stx)
  (raise-syntax-error #f "no value for positional variable" stx))

;; An alternate is (cons pattern action-expr)
;; An alternate* is (cons pattern action-function-name)

(define-for-syntax ((make-production-splitter okW intW) stx)
  (syntax-case stx ()
    [(_ (name form ...))
     (let ()
       (define-values (options alternates0)
         (partition-options/alternates (syntax->list #'(form ...))))
       (define wrap?
         (let ([wrap? (assq '#:wrap options)]
               [no-wrap? (assq '#:no-wrap options)])
           (when (and wrap? no-wrap?)
             (raise-syntax-error 'split
                                 "cannot specify both #:wrap and #:no-wrap"
                                 stx))
           #;
           (unless (and (or wrap? no-wrap?) (not (and wrap? no-wrap?)))
             (raise-syntax-error 'split
                                 "must specify exactly one of #:wrap, #:no-wrap"
                                 stx))
           (and wrap? #t)))
       (define args-spec
         (let ([p (assq '#:args options)]) (and p (cdr p))))
       (define rewrite-alt+def (make-rewrite-alt+def #'name args-spec))
       (define alternates+definitions
         (map rewrite-alt+def alternates0 (build-list (length alternates0) add1)))
       (define alternates (map car alternates+definitions))
       (define action-definitions (map cdr alternates+definitions))
       (define elaborate-successful-alternate
         (make-elaborate-successful-alternate wrap? okW))
       (define elaborate-interrupted-alternate
         (make-elaborate-interrupted-alternate wrap? intW))
       (define successful-alternates
         (apply append (map elaborate-successful-alternate alternates)))
       (define interrupted-alternates 
         (apply append (map elaborate-interrupted-alternate alternates)))
       (with-syntax ([((success-pattern . success-action) ...)
                      successful-alternates]
                     [((interrupted-pattern . interrupted-action) ...)
                      interrupted-alternates]
                     [skip-spec (assq '#:skipped options)]
                     [args-spec (assq '#:args options)]
                     [name/Skipped (skipped-name #'name)]
                     [name/Interrupted (interrupted-name #'name)]
                     [%action ((syntax-local-certifier) #'%action)])
         #`(begin
             (definitions #,@action-definitions)
             (productions
              (name [success-pattern success-action] ...)
              #,(if (pair? interrupted-alternates)
                    #'(name/Interrupted [interrupted-pattern interrupted-action]
                                        ...)
                    #'(name/Interrupted [(IMPOSSIBLE) #f]))
              (name/Skipped [() (%skipped args-spec skip-spec)])))))]))

(define-syntax (skipped-token-values stx)
  (syntax-case stx ()
    [(skipped-token-values)
     #'(begin)]
    [(skipped-token-values name . more)
     (identifier? #'name)
     (with-syntax ([name/Skipped (skipped-name #'name)])
       #'(begin (productions (name/Skipped [() #f]))
                (skipped-token-values . more)))]
    [(skipped-token-values (name value) . more)
     (with-syntax ([name/Skipped (skipped-name #'name)])
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
    [(%action (#:args . args) action)
     #'(lambda args action)]
    [(%action #f action)
     #'action]))
