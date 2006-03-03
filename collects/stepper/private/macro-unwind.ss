(module macro-unwind mzscheme
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "etc.ss")
           (lib "contract.ss")
           "shared.ss"
           "lifting.ss")

  (provide/contract [unwind (syntax? boolean? . -> . (listof syntax?))]
                    [unwind-no-highlight (syntax? . -> . (listof syntax?))])

                                                                ;              ;  ;
                                                                               ;
 ; ;;; ;;    ;;;    ;;;  ; ;;  ;;;       ;   ;  ; ;;  ;   ;   ; ;  ; ;;    ;;; ;  ;  ; ;;    ;; ;
 ;;  ;;  ;  ;   ;  ;     ;;   ;   ;      ;   ;  ;;  ; ;   ;   ; ;  ;;  ;  ;   ;;  ;  ;;  ;  ;  ;;
 ;   ;   ;      ;  ;     ;    ;   ;      ;   ;  ;   ;  ; ; ; ;  ;  ;   ;  ;    ;  ;  ;   ;  ;   ;
 ;   ;   ;   ;;;;  ;     ;    ;   ;      ;   ;  ;   ;  ; ; ; ;  ;  ;   ;  ;    ;  ;  ;   ;  ;   ;
 ;   ;   ;  ;   ;  ;     ;    ;   ;      ;   ;  ;   ;  ; ; ; ;  ;  ;   ;  ;    ;  ;  ;   ;  ;   ;
 ;   ;   ;  ;   ;  ;     ;    ;   ;      ;  ;;  ;   ;  ; ; ; ;  ;  ;   ;  ;   ;;  ;  ;   ;  ;  ;;
 ;   ;   ;   ;;;;;  ;;;  ;     ;;;        ;; ;  ;   ;   ;   ;   ;  ;   ;   ;;; ;  ;  ;   ;   ;; ;
                                                                                                ;

  ; unwind takes a syntax object with a single highlight,
  ; and returns a list of syntax objects

  (define (unwind stx lift-at-highlight?)
    (macro-unwind (lift stx lift-at-highlight?)))

  ; unwind-no-highlight is really just macro-unwind, but with the 'right'
  ; interface that makes it more obvious what it does.
  ; [unwind-no-highlight (-> syntax? (listof syntax?))]

  (define (unwind-no-highlight stx)
    (macro-unwind (list stx)))

  ; unwind-only-highlight : syntax? -> (listof syntax?)
  (define (unwind-only-highlight stx)
    (unwind stx #t))

  (define (improper-member elt improper-list)
    (cond [(pair? improper-list)
           (or (eq? elt (car improper-list))
               (improper-member elt (cdr improper-list)))]
          [else
           (eq? elt improper-list)]))

  (define-syntax (noisy-and stx)
    (syntax-case stx ()
      [(_) #`#t]
      [(_ a b ...)
       (with-syntax ([inner (syntax/loc stx (noisy-and b ...))]
                     [error (syntax/loc #`a
                              (error 'noisy-and "and clause failed"))])
       (syntax/loc stx (if a inner error)))]
      [else
       (error 'noisy-and "bad syntax for noisy-and")]))

  ;(->* (syntax? (listof syntax?))
  ;     (syntax? (listof syntax?)))

  (define (macro-unwind stxs)
    (local
        ((define (recur-on-pieces stx)
           (if (pair? (syntax-e stx))
             (datum->syntax-object
              stx (syntax-pair-map (syntax-e stx) inner) stx stx)
             stx))

         (define (inner stx)
           (define (fall-through)
             (kernel:kernel-syntax-case stx #f
               [id
                (identifier? stx)
                (or (syntax-property stx 'stepper-lifted-name)
                    stx)]
               [(define-values dc ...)
                (unwind-define stx)]
               [(#%app exp ...)
                (recur-on-pieces #'(exp ...))]
               [(#%datum . datum)
                #'datum]
               [(let-values . rest)
                (unwind-mz-let stx)]
               [(letrec-values . rest)
                (unwind-mz-let stx)]
               [(set! var rhs)
                (with-syntax ([unwound-var (or (syntax-property
                                                #`var 'stepper-lifted-name)
                                               #`var)]
                              [unwound-body (inner #`rhs)])
                  #`(set! unwound-var unwound-body))]
               [else
                (recur-on-pieces stx)]))

           (transfer-info
            (if (syntax-property stx 'user-stepper-hint)
                (case (syntax-property stx 'user-stepper-hint)

                  [(comes-from-cond)
                   (unwind-cond stx
                                (syntax-property stx 'user-source)
                                (syntax-property stx 'user-position))]

                  [(comes-from-and)
                   (unwind-and/or stx
                                  (syntax-property stx 'user-source)
                                  (syntax-property stx 'user-position)
                                  'and)]

                  [(comes-from-or)
                   (unwind-and/or stx
                                  (syntax-property stx 'user-source)
                                  (syntax-property stx 'user-position)
                                  'or)]

                  [(comes-from-local)
                   (unwind-local stx)]

                  [(comes-from-recur)
                   (unwind-recur stx)]

                  [(comes-from-begin)
                   (unwind-begin stx)]

                  (else (fall-through)))
                (fall-through))
            stx))

         (define (transfer-highlight from to)
           (if (syntax-property from 'stepper-highlight)
               (syntax-property to 'stepper-highlight #t)
               to))

         (define (unwind-recur stx)
           ;; if you use #%app, it gets captured here
           (with-syntax ([(app-keywd letrec-term argval ...) stx])
             (with-syntax ([(new-argval ...)
                            (map inner (syntax->list #`(argval ...)))])
               (let ([unwound (inner #`letrec-term)])
                 (syntax-case unwound (letrec lambda)
                   [(letrec ([loop-name (lambda (argname ...) . bodies)])
                      loop-name-2)
                    (unless (module-identifier=? #`loop-name #`loop-name-2)
                      (error "unexpected syntax for 'recur': ~v" stx))
                    (transfer-highlight
                     unwound
                     #`(recur loop-name ([argname new-argval] ...) . bodies))]
                   [else #`(#,unwound new-argval ...)])))))

         (define (unwind-define stx)
           (kernel:kernel-syntax-case stx #f
             [(define-values (name . others) body)
              (begin
                (unless (null? (syntax-e #'others))
                  (error 'reconstruct
                         "reconstruct fails on multiple-values define: ~v\n"
                         (syntax-object->datum stx)))
                (let* ([printed-name
                        (or (syntax-property #`name 'stepper-lifted-name)
                            (syntax-property #'name 'stepper-orig-name)
                            #'name)]
                       [unwound-body (inner #'body)]
                       ;; see notes in internal-docs.txt
                       [define-type (syntax-property
                                     unwound-body 'user-stepper-define-type)])
                  (if define-type
                      (kernel:kernel-syntax-case unwound-body #f
                        [(lambda arglist lam-body ...)
                         (case define-type
                           [(shortened-proc-define)
                            (let ([proc-define-name
                                   (syntax-property
                                    unwound-body
                                    'user-stepper-proc-define-name)])
                              (if (or (module-identifier=? proc-define-name
                                                           #'name)
                                      (and (syntax-property #'name
                                                            'stepper-orig-name)
                                           (module-identifier=?
                                            proc-define-name
                                            (syntax-property
                                             #'name 'stepper-orig-name))))
                                  #`(define (#,printed-name . arglist)
                                      lam-body ...)
                                  #`(define #,printed-name
                                      #,unwound-body)))]
                           [(lambda-define)
                            #`(define #,printed-name #,unwound-body)]
                           [else (error 'unwind-define
                                        "unknown value for syntax property 'user-stepper-define-type: ~e"
                                        define-type)])]
                        [else (error 'unwind-define
                                     "expr with stepper-define-type is not a lambda: ~e"
                                     (syntax-object->datum unwound-body))])
                      #`(define #,printed-name #,unwound-body))))]
             [else (error 'unwind-define
                          "expression is not a define-values: ~e"
                          (syntax-object->datum stx))]))

         (define (unwind-mz-let stx)
           (with-syntax ([(label ([(var) rhs] ...) . bodies) stx])
             (with-syntax ([(rhs2 ...) (map inner (syntax->list #'(rhs ...)))]
                           [new-label
                            (if (improper-member 'comes-from-let*
                                                 (syntax-property
                                                  stx 'user-stepper-hint))
                              #`let*
                              (case (syntax-e #'label)
                                [(let-values) #'let]
                                [(letrec-values) #'letrec]))]
                           [new-bodies (map inner (syntax->list #'bodies))])
               ;; is this let and the nested one part of a let*?
               (syntax-case #`new-bodies (let*)
                 [((let* bindings inner-body ...))
                  (and
                   (improper-member 'comes-from-let*
                                    (syntax-property stx 'user-stepper-hint))
                   (eq? (syntax-property stx 'user-stepper-source)
                        (syntax-property (car (syntax->list #`new-bodies))
                                         'user-stepper-source))
                   (eq? (syntax-property stx 'user-stepper-position)
                        (syntax-property (car (syntax->list #`new-bodies))
                                         'user-stepper-position)))
                  #`(let* #,(append (syntax->list #`([var rhs2] ...))
                                    (syntax->list #`bindings))
                      inner-body ...)]
                 [else
                  #`(new-label ([var rhs2] ...) . new-bodies)]))))

         (define (unwind-local stx)
           (kernel:kernel-syntax-case stx #f
             ;; at least through intermediate, define-values may not occur in
             ;; local.
             [(letrec-values ([vars exp] ...) body)
              (with-syntax ([defns (map inner
                                        (syntax->list
                                         #`((define-values vars exp) ...)))])
                  #`(local defns #,(inner #'body)))]
             [else (error 'unwind-local
                          "expected a letrec-values, given: ~e"
                          (syntax-object->datum stx))]))

         ;(define (unwind-quasiquote-the-cons-application stx)
         ;  (syntax-case (recur-on-pieces stx) ()
         ;    [(#%app the-cons . rest)
         ;     (syntax (cons . rest))]
         ;    [else
         ;     (error 'reconstruct
         ;            "unexpected result for unwinding the-cons application")]))

         (define (unwind-cond-clause stx test-stx result-stx)
           (with-syntax ([new-test (if (syntax-property stx 'user-stepper-else)
                                       #`else
                                       (inner test-stx))]
                         [result (inner result-stx)])
             #`(new-test result)))

         (define (unwind-cond stx user-source user-position)
           (with-syntax
               ([clauses
                 (let loop ([stx stx])
                   (if (and (eq? user-source
                                 (syntax-property stx 'user-source))
                            (eq? user-position
                                 (syntax-property stx 'user-position)))
                     (syntax-case stx (if begin #%app)
                       ;; the else clause disappears when it's a
                       ;; language-inserted else clause
                       [(if test result)
                        (list (unwind-cond-clause stx #`test #`result))]
                       [(if test result else-clause)
                        (cons (unwind-cond-clause stx #`test #`result)
                              (loop (syntax else-clause)))]
                       ;; else clause appears momentarily in 'before,' even
                       ;; though it's a 'skip-completely'
                       [(begin . rest) null]
                       [else-stx
                        (error 'unwind-cond
                               "expected an if, got: ~e"
                               (syntax-object->datum (syntax else-stx)))])
                     (error 'unwind-cond
                            "expected a cond clause expansion, got: ~e"
                            (syntax-object->datum stx))))])
             (syntax (cond . clauses))))

         (define (unwind-begin stx)
           (syntax-case stx (let-values)
             [(let-values () body ...)
              (with-syntax ([(new-body ...)
                             (map inner (syntax->list #`(body ...)))])
                #`(begin new-body ...))]))

         (define (unwind-and/or stx user-source user-position label)
           (let ([clause-padder (case label
                                  [(and) #`true]
                                  [(or) #`false])])
             (with-syntax
                 ([clauses
                   (append
                    (build-list (syntax-property
                                 stx 'user-stepper-and/or-clauses-consumed)
                                (lambda (dc) clause-padder))
                    (let loop ([stx stx])
                      (if (and (eq? user-source
                                    (syntax-property stx 'user-source))
                               (eq? user-position
                                    (syntax-property stx 'user-position)))
                        (syntax-case stx (if let-values #%datum)
                          [(if part-1 part-2 part-3)
                           (cons (inner (syntax part-1))
                                 (case label
                                   [(and) (loop (syntax part-2))]
                                   [(or) (loop (syntax part-3))]
                                   [else (error 'unwind-and/or
                                                "unknown label ~a" label)]))]
                          [else
                           (error 'unwind-and/or
                                  "syntax: ~a does not match and/or patterns"
                                  (syntax-object->datum stx))])
                        null)))])
               #`(#,label . clauses)))))

      (map inner stxs))))
