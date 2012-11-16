#lang racket/base

(require (only-in syntax/kerncase kernel-syntax-case)
         racket/list
         racket/contract
         "model-settings.rkt"
         "shared.rkt"
         "syntax-property.rkt")

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

;(->* (syntax? (listof syntax?))
;     (syntax? (listof syntax?)))

(define (recur-on-pieces stx settings)
  (if (pair? (syntax-e stx))
      (datum->syntax
       stx (syntax-pair-map (syntax-e stx) (lambda (stx) (unwind stx settings))) stx stx)
      stx))

(define (fall-through stx settings)
  (kernel-syntax-case stx #f
    [id
     (identifier? stx)
     (or (stepper-syntax-property stx 'stepper-lifted-name)
         stx)]
    [(define-values dc ...)
     (unwind-define stx settings)]
    ; STC: app special cases from lazy racket
    ; extract-if-lazy-proc - can't hide this in lazy.rkt bc it's needed
    ; to distinguish the general lazy application
    [(#%plain-app proc-extract p)
     (or (eq? (syntax->datum #'proc-extract) 'extract-if-lazy-proc)
         (eq? (object-name 
               (with-handlers ; for print output-style
                   ([(λ (e) #t) (λ (e) #f)]) 
                 (syntax-e (second (syntax-e #'proc-extract)))))
              'extract-if-lazy-proc))
     (unwind #'p settings)]
    ; lazy #%app special case: force and delay
    [(#%plain-app f arg)
     (let ([fn (syntax->datum #'f)])
       (or (eq? fn 'lazy-proc)
           (eq? fn 'make-lazy-proc)
           (eq? fn 'force) (eq? fn '!) (eq? fn '!!)
           (eq? fn '!list) (eq? fn '!!list)
           (equal? fn '(#%plain-app parameter-procedure))))
     (unwind #'arg settings)]
    ; general lazy application
    [(#%plain-app 
      (#%plain-lambda args1 (#%plain-app (#%plain-app proc p) . args2)) 
      . args3)
     (and (eq? (syntax->datum #'proc) 'extract-if-lazy-proc)
          (equal? (syntax->datum (cdr (syntax-e #'args1)))
                  (syntax->datum #'args2)))
     (recur-on-pieces #'args3 settings)]
    [(#%plain-app exp ...)
     (recur-on-pieces #'(exp ...) settings)]
    [(quote datum)
     (if (symbol? #'datum)
         stx
         #'datum)]
    [(let-values . rest)
     (unwind-mz-let stx settings)]
    [(letrec-values . rest)
     (unwind-mz-let stx settings)]
    [(#%plain-lambda . rest)
     (recur-on-pieces #'(lambda . rest) settings)]
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
         ; STC: For fn hints, I changed the recur procedure to unwind
         ; (was recur-on-pieces). This should not affect the non-lazy 
         ; stepper since it doesnt seem to use any fn hints.
         (hint stx (lambda (stx) (unwind stx settings)))
         (let ([process (case hint
                          [(comes-from-cond)  unwind-cond]
                          [(comes-from-and)   (unwind-and/or 'and)]
                          [(comes-from-or)    (unwind-and/or 'or)]
                          [(comes-from-local) unwind-local]
                          [(comes-from-recur) unwind-recur]
                          [(comes-from-check-expect) unwind-check-expect]
                          [(comes-from-check-within) unwind-check-within]
                          [(comes-from-check-error) unwind-check-error]
                          ;; unused: the fake-exp begin takes care of this for us...
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
           (unless (free-identifier=? #`loop-name #`loop-name-2)
             (error "unexpected syntax for 'recur': ~v" stx))
           (transfer-highlight
            unwound
            #`(recur loop-name ([argname new-argval] ...) . bodies))]
          [else #`(#,unwound new-argval ...)])))))

(define (unwind-define stx settings)
  (kernel-syntax-case stx #f
    [(define-values (name . others) body)
     (if (null? (syntax-e #'others))
         ;; this is supported:
         (let* ([printed-name
                 (or (stepper-syntax-property #`name 'stepper-lifted-name)
                     (stepper-syntax-property #'name 'stepper-orig-name)
                     #'name)]
                [unwound-body (unwind #'body settings)]
                ;; see notes in internal-docs.txt
                [define-type (stepper-syntax-property
                              unwound-body 'stepper-define-type)])
           (if define-type
               (kernel-syntax-case 
                   unwound-body #f
                 [(lambda arglist lam-body ...)
                  (case define-type
                    [(shortened-proc-define)
                     (let ([proc-define-name
                            (stepper-syntax-property
                             unwound-body
                             'stepper-proc-define-name)])
                       (if (or (free-identifier=? proc-define-name
                                                  #'name)
                               (and (stepper-syntax-property #'name
                                                             'stepper-orig-name)
                                    (free-identifier=?
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
                              "expr with stepper-define-type is not a lambda: ~.s"
                              (syntax->datum unwound-body))])
               #`(define #,printed-name #,unwound-body)))
         ;; this is there just to see the unsupported stuff go by...
         #`(define-values (name . others) #,(unwind #'body settings))
         )]
    [else (error 'unwind-define
                 "expression is not a define-values: ~.s"
                 (syntax->datum stx))]))

(define (unwind-mz-let stx settings)
  (syntax-case stx ()
    [(label ([(var) rhs] ...) . bodies)
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
       ;; is there a nested let-form that should be combined with this one?
       #;(syntax-case #`new-bodies ()
         [(only-body)
          (same-source? stx #'only-body)
          (syntax-case #'only-body ()
            [(let/*/rec bindings inner-body ...)
             ]
            [else 
             (error "internal error 20110709: nested expr in let/*/rec had same source, but wasn't a let/*/rec")])])
       ;; is this let and the nested one part of a let*?
       (syntax-case #`new-bodies (let*)
         [((let* bindings inner-body ...))
          (and
           (improper-member 'comes-from-let*
                            (stepper-syntax-property stx 'stepper-hint))
           (same-source? stx (car (syntax->list #`new-bodies))))
          #`(let* #,(append (syntax->list #`([var rhs2] ...))
                            (syntax->list #`bindings))
              inner-body ...)]
         [else
          #`(new-label ([var rhs2] ...) . new-bodies)]))]
    [;; it's not part of the language we support... might as well just blow it on out there
     (label ([(var ...) rhs] ...) . bodies)
     (with-syntax ([(rhs2 ...) (map (lambda (rhs) (unwind rhs settings)) (syntax->list #'(rhs ...)))]
                   [new-bodies (map (lambda (body) (unwind body settings)) (syntax->list #'bodies))])
       #`(label ([(var ...) rhs2] ...) . new-bodies))]))

(define (unwind-local stx settings)
  (kernel-syntax-case stx #f
    ;; at least through intermediate, define-values may not occur in
    ;; local.
    [(letrec-values ([vars exp] ...) body)
     (with-syntax ([defns (map (lambda (def)
                                 (unwind def settings))
                               (syntax->list
                                #`((define-values vars exp) ...)))])
       #`(local defns #,(unwind #'body settings)))]
    [else (error 'unwind-local
                 "expected a letrec-values, given: ~.s"
                 (syntax->datum stx))]))

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
  (let ([outer-stx stx])
    (with-syntax
        ([clauses
          (let loop ([stx stx])
            (if (and (same-source? outer-stx stx))
                (syntax-case stx (if begin let-values)
                  ;; the else clause disappears when it's a
                  ;; language-inserted else clause
                  [(if test result)
                   (list (unwind-cond-clause stx #`test #`result settings))]                     
                  [(if test result else-clause)
                   (cons (unwind-cond-clause stx #`test #`result settings)
                         (loop (syntax else-clause)))]
                  ;; else clause appears momentarily in 'before,' even
                  ;; though it's a 'skip-completely'
                  [(let-values () . rest) null]
                  [else-stx
                   (error 'unwind-cond
                          "expected an if, got: ~.s"
                          (syntax->datum (syntax else-stx)))])
                (error 'unwind-cond
                       "expected a cond clause expansion, got: ~.s"
                       (syntax->datum stx))))])
      (syntax (cond . clauses)))))

(define ((unwind-and/or label) stx settings)
  (let ([user-source   (syntax-property stx 'user-source)]
        [user-position (syntax-property stx 'user-position)])
    (with-syntax
        ([clauses
          (append
           (if (render-settings-show-and/or-clauses-consumed? settings)
               (build-list (stepper-syntax-property
                            stx 'stepper-and/or-clauses-consumed)
                           (let ([clause-padder
                                  (if (render-settings-true-false-printed? settings)
                                      (case label [(and) #'true] [(or) #'false])
                                      (case label [(and) #'#t] [(or) #'#f]))])
                             (lambda (dc) clause-padder)))
               '())
           (let loop ([stx stx])
             (if (and (eq? user-source
                           (syntax-property stx 'user-source))
                      (eq? user-position
                           (syntax-property stx 'user-position)))
                 (syntax-case stx (if)
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
                           (syntax->datum stx))])
                 null)))])
      #`(#,label . clauses))))

(define (unwind-check-expect stx settings)
  (kernel-syntax-case (fall-through stx settings) #f
    [(c-e (lambda () a1) a2 a3 a4)
     #`(check-expect a1 a2)]
    [(dots1 actual dots2) 
     (and (eq? (syntax->datum #'dots1) '...)
          (eq? (syntax->datum #'dots2) '...))
     (with-syntax ([expected (unwind (third (stepper-syntax-property stx 'stepper-args-of-call)) settings)])
       #`(check-expect actual expected))]
    [any #`(c-e any) #;#`(check-expect )]))

(define (unwind-check-within stx settings)
  (kernel-syntax-case (fall-through stx settings) #f
    [(c-e (lambda () a1) a2 a3 a4 a5)
     #`(check-within a1 a2 a3)]
    [(dots1 actual dots2)
     (and (eq? (syntax->datum #'dots1) '...)
          (eq? (syntax->datum #'dots2) '...))
     (let ([args-of-call (stepper-syntax-property stx 'stepper-args-of-call)])
       (with-syntax ([expected (unwind (third args-of-call) settings)]
                     [within (unwind (fourth args-of-call) settings)])
         #`(check-within actual expected within)))]
    [any #`(c-e any) #;#`(check-expect )]))

(define (unwind-check-error stx settings)
  (kernel-syntax-case (fall-through stx settings) #f
    [(c-e (lambda () a1) a2 a3 a4)
     #`(check-error a1 a2)]
    [(dots1 actual dots2)
     (and (eq? (syntax->datum #'dots1) '...)
          (eq? (syntax->datum #'dots2) '...))
     (let ([args-of-call (stepper-syntax-property stx 'stepper-args-of-call)])
       (with-syntax ([expected (unwind (third args-of-call) settings)])
         #`(check-error actual expected)))]
    [any #`(c-e any) #;#`(check-expect )]))

(define (same-source? stx1 stx2)
  (and (equal? (syntax-property stx1 'user-source)
               (syntax-property stx2 'user-source))
       (equal? (syntax-property stx1 'user-position)
               (syntax-property stx2 'user-position))))
