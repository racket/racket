 (module macro-unwind mzscheme
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "etc.ss")
           (lib "contract.ss")
	   "model-settings.ss"
           "shared.ss")

  (provide/contract [unwind (syntax? render-settings? . -> . syntax?)])
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

  (define (recur-on-pieces stx settings)
     (if (pair? (syntax-e stx))
         (datum->syntax-object
          stx (syntax-pair-map (syntax-e stx) (lambda (stx) (unwind stx settings))) stx stx)
         stx))
   
   (define (fall-through stx settings)
     (kernel:kernel-syntax-case stx #f
                                [id
                                 (identifier? stx)
                                 (or (stepper-syntax-property stx 'stepper-lifted-name)
                                     stx)]
                                [(define-values dc ...)
                                 (unwind-define stx settings)]
                                [(#%app exp ...)
                                 (recur-on-pieces #'(exp ...) settings)]
                                [(#%datum . datum)
                                 #'datum]
                                [(let-values . rest)
                                 (unwind-mz-let stx settings)]
                                [(letrec-values . rest)
                                 (unwind-mz-let stx settings)]
                                [(set! var rhs)
                                 (with-syntax ([unwound-var (or (stepper-syntax-property
                                                                 #`var 'stepper-lifted-name)
                                                                #`var)]
                                               [unwound-body (unwind #`rhs settings)])
                                   #`(set! unwound-var unwound-body))]
                                [else (recur-on-pieces stx settings)]))
   
   (define (unwind stx settings)
     (transfer-info
      (let ([hint (stepper-syntax-property stx 'stepper-hint)])
        (if (procedure? hint)
            (hint stx (lambda (stx) (recur-on-pieces stx settings)))
            (let ([process (case hint
                             [(comes-from-cond)  unwind-cond]
                             [(comes-from-and)   (unwind-and/or 'and)]
                             [(comes-from-or)    (unwind-and/or 'or)]
                             [(comes-from-local) unwind-local]
                             [(comes-from-recur) unwind-recur]
                             ;;[(comes-from-begin) unwind-begin]
                             [else fall-through])])
              (process stx settings))))
      stx))
   
   (define (transfer-highlight from to)
     (if (stepper-syntax-property from 'stepper-highlight)
         (stepper-syntax-property to 'stepper-highlight #t)
         to))
   
   (define (unwind-recur stx settings)
     ;; if you use #%app, it gets captured here
     (with-syntax ([(app-keywd letrec-term argval ...) stx])
       (with-syntax ([(new-argval ...)
                      (map (lambda (argval) (unwind argval settings)) (syntax->list #`(argval ...)))])
         (let ([unwound (unwind #`letrec-term settings)])
           (syntax-case unwound (letrec lambda)
             [(letrec ([loop-name (lambda (argname ...) . bodies)])
                loop-name-2)
              (unless (module-identifier=? #`loop-name #`loop-name-2)
                (error "unexpected syntax for 'recur': ~v" stx))
              (transfer-highlight
               unwound
               #`(recur loop-name ([argname new-argval] ...) . bodies))]
             [else #`(#,unwound new-argval ...)])))))
   
   (define (unwind-define stx settings)
     (kernel:kernel-syntax-case stx #f
                                [(define-values (name . others) body)
                                 (begin
                                   (unless (null? (syntax-e #'others))
                                     (error 'reconstruct
                                            "reconstruct fails on multiple-values define: ~v\n"
                                            (syntax-object->datum stx)))
                                   (let* ([printed-name
                                           (or (stepper-syntax-property #`name 'stepper-lifted-name)
                                               (stepper-syntax-property #'name 'stepper-orig-name)
                                               #'name)]
                                          [unwound-body (unwind #'body settings)]
                                          ;; see notes in internal-docs.txt
                                          [define-type (stepper-syntax-property
                                                        unwound-body 'stepper-define-type)])
                                     (if define-type
                                         (kernel:kernel-syntax-case unwound-body #f
                                                                    [(lambda arglist lam-body ...)
                                                                     (case define-type
                                                                       [(shortened-proc-define)
                                                                        (let ([proc-define-name
                                                                               (stepper-syntax-property
                                                                                unwound-body
                                                                                'stepper-proc-define-name)])
                                                                          (if (or (module-identifier=? proc-define-name
                                                                                                       #'name)
                                                                                  (and (stepper-syntax-property #'name
                                                                                                        'stepper-orig-name)
                                                                                       (module-identifier=?
                                                                                        proc-define-name
                                                                                        (stepper-syntax-property
                                                                                         #'name 'stepper-orig-name))))
                                                                              #`(define (#,printed-name . arglist)
                                                                                  lam-body ...)
                                                                              #`(define #,printed-name
                                                                                  #,unwound-body)))]
                                                                       [(lambda-define)
                                                                        #`(define #,printed-name #,unwound-body)]
                                                                       [else (error 'unwind-define
                                                                                    "unknown value for syntax property 'stepper-define-type: ~e"
                                                                                    define-type)])]
                                                                    [else (error 'unwind-define
                                                                                 "expr with stepper-define-type is not a lambda: ~e"
                                                                                 (syntax-object->datum unwound-body))])
                                         #`(define #,printed-name #,unwound-body))))]
                                [else (error 'unwind-define
                                             "expression is not a define-values: ~e"
                                             (syntax-object->datum stx))]))
   
   (define (unwind-mz-let stx settings)
     (with-syntax ([(label ([(var) rhs] ...) . bodies) stx])
       (with-syntax ([(rhs2 ...) (map (lambda (rhs) (unwind rhs settings)) (syntax->list #'(rhs ...)))]
                     [new-label
                      (if (improper-member 'comes-from-let*
                                           (stepper-syntax-property
                                            stx 'stepper-hint))
                          #`let*
                          (case (syntax-e #'label)
                            [(let-values) #'let]
                            [(letrec-values) #'letrec]))]
                     [new-bodies (map (lambda (body) (unwind body settings)) (syntax->list #'bodies))])
         ;; is this let and the nested one part of a let*?
         (syntax-case #`new-bodies (let*)
           [((let* bindings inner-body ...))
            (and
             (improper-member 'comes-from-let*
                              (stepper-syntax-property stx 'stepper-hint))
             (eq? (stepper-syntax-property stx 'stepper-source)
                  (stepper-syntax-property (car (syntax->list #`new-bodies))
                                   'stepper-source))
             (eq? (stepper-syntax-property stx 'stepper-position)
                  (stepper-syntax-property (car (syntax->list #`new-bodies))
                                   'stepper-position)))
            #`(let* #,(append (syntax->list #`([var rhs2] ...))
                              (syntax->list #`bindings))
                inner-body ...)]
           [else
            #`(new-label ([var rhs2] ...) . new-bodies)]))))
   
   (define (unwind-local stx settings)
     (kernel:kernel-syntax-case stx #f
                                ;; at least through intermediate, define-values may not occur in
                                ;; local.
                                [(letrec-values ([vars exp] ...) body)
                                 (with-syntax ([defns (map (lambda (def)
							     (unwind def settings))
                                                           (syntax->list
                                                            #`((define-values vars exp) ...)))])
                                   #`(local defns #,(unwind #'body settings)))]
                                [else (error 'unwind-local
                                             "expected a letrec-values, given: ~e"
                                             (syntax-object->datum stx))]))
   
   ;(define (unwind-quasiquote-the-cons-application stx settings)
   ;  (syntax-case (recur-on-pieces stx settings) ()
   ;    [(#%app the-cons . rest)
   ;     (syntax (cons . rest))]
   ;    [else
   ;     (error 'reconstruct
   ;            "unexpected result for unwinding the-cons application")]))
   
   (define (unwind-cond-clause stx test-stx result-stx settings)
     (with-syntax ([new-test (if (stepper-syntax-property stx 'stepper-else)
                                 #`else
                                 (unwind test-stx settings))]
                   [result (unwind result-stx settings)])
       #`(new-test result)))
   
   (define (unwind-cond stx settings)
     (let ([user-source   (syntax-property stx 'user-source)]
           [user-position (syntax-property stx 'user-position)])
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
                      (list (unwind-cond-clause stx #`test #`result settings))]
                     [(if test result else-clause)
                      (cons (unwind-cond-clause stx #`test #`result settings)
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
         (syntax (cond . clauses)))))
   
  ;; unused: the fake-exp begin takes care of this for us...
  #;(define (unwind-begin stx settings)
      (syntax-case stx (let-values)
        [(let-values () body ...)
         (with-syntax ([(new-body ...)
                        (map (lambda (body) (unwind body settings)) (syntax->list #`(body ...)))])
           #`(begin new-body ...))]))
   
  (define ((unwind-and/or label) stx settings)
    (let ([user-source   (syntax-property stx 'user-source)]
          [user-position (syntax-property stx 'user-position)]
          [clause-padder
 	   (if (render-settings-true-false-printed? settings)
 	       (case label [(and) #'true] [(or) #'false])
 	       (case label [(and) #'#t] [(or) #'#f]))])
      (with-syntax
          ([clauses
            (append
             (build-list (stepper-syntax-property
                          stx 'stepper-and/or-clauses-consumed)
                         (lambda (dc) clause-padder))
             (let loop ([stx stx])
               (if (and (eq? user-source
                             (syntax-property stx 'user-source))
                        (eq? user-position
                             (syntax-property stx 'user-position)))
                   (syntax-case stx (if let-values #%datum)
                     [(if part-1 part-2 part-3)
                      (cons (unwind (syntax part-1) settings)
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
        #`(#,label . clauses))))
)
