
;;----------------------------------------------------------------------
;; -define, when, unless, let/ec, define-struct

(module define-et-al '#%kernel
  (#%require (for-syntax '#%kernel "stx.rkt" "qq-and-or.rkt" 
                         "cond.rkt"))

  (#%provide -define -define-syntax
             when unless
             call/ec let/ec)

  ; --------------------------------------------------
  ;
  ;
  ;        ;             ;;;;   ;                                        ;                                    ;                 ;
  ;        ;            ;                                               ;                                     ;                  ;
  ;        ;            ;                                              ;                                      ;                   ;
  ;     ;;;;    ;;;     ;     ;;;     ; ;;;     ;;;                    ;      ; ;;;     ;;    ; ;;;           ;   ;  ;  ;  ;      ;
  ;    ;   ;   ;   ;  ;;;;;;    ;     ;;   ;   ;   ;                  ;       ;;   ;   ;  ;   ;;   ;          ;  ;   ;  ;  ;       ;
  ;   ;    ;  ;    ;    ;       ;     ;    ;  ;    ;                  ;       ;    ;  ;    ;  ;    ;          ; ;    ; ; ; ;       ;
  ;   ;    ;  ;;;;;;    ;       ;     ;    ;  ;;;;;;                  ;       ;    ;  ;    ;  ;    ;          ;;     ; ; ; ;       ;
  ;   ;    ;  ;         ;       ;     ;    ;  ;                       ;       ;    ;  ;    ;  ;    ;          ;;     ; ; ; ;       ;
  ;   ;    ;  ;         ;       ;     ;    ;  ;                        ;      ;    ;  ;    ;  ;    ;          ; ;    ; ; ; ;      ;
  ;   ;   ;;   ;        ;       ;     ;    ;   ;                       ;      ;    ;   ;  ;   ;    ;          ;  ;    ;   ;       ;
  ;    ;;; ;    ;;;;    ;       ;;;   ;    ;    ;;;;                    ;     ;    ;    ;;    ;    ;          ;   ;   ;   ;      ;
  ;                                                  ;;;;;;;             ;                                                      ;
  ;                                                                       ;                                                    ;
  ;
  ;
  ; non-keyword define* forms
  ;

  
  ; define, define-syntax, and define-values-for-syntax
  ;
  ; These are not directly user-visible, in the sense that they get replaced by
  ; the keyword define at the last step of racket/base. However, they are used
  ; pretty extensively, and I'm not sure that one isn't, say, included in the
  ; expansion of some macro such that errors end up being user-visible. So,
  ; we put in the effort to get good error messages.
  (begin-for-syntax
    (define-values (check-procedure-arg-name)
      (lambda (full-stx arg-name)
        (raise-syntax-error-unless (identifier? arg-name)
                                   "bad syntax (not an identifier for procedure argument)"
                                   full-stx
                                   arg-name)))

    (define-values (check-procedure-arg-spec)
      (lambda (full-stx arg-spec)
        (if (stx-pair? arg-spec)
            (begin (check-procedure-arg-name full-stx (stx-car arg-spec))
                   (check-procedure-arg-spec full-stx (stx-cdr arg-spec)))
            (if (stx-null? arg-spec)
                (void)
                (check-procedure-arg-name full-stx arg-spec)))))

    ; (process-function-define full-stx orig-prototype-stx orig-body-stxlist)
    ;   full-stx           = #'(define orig-prototype . body-stxlist)
    ;   orig-prototype-stx : #'(inner-id-or-prototype . arg-spec)
    ;   orig-body-stxlist  : stx-list?
    ;  -> (values identifier? syntax?)
    ; Given the user-specified id and body for an function define. Validates
    ; and then returns the identifier and expression rhs for the final output
    ; define-values (or define-syntaxes).
    (define-values (process-function-define)
      (lambda (full-stx orig-prototype-stx orig-body-stxlist)
        ; We iteratively unwrap the current prototype and wrap the body at
        ; the same time. Along the way, we build up a list of the arg-specs
        ; along the way and validate them all at the end, which gives us
        ; left-to-right error reporting. The iteration step is
        ;
        ;     cur-prototype-stx               cur-wrapped-body-list                          queued-arg-specs
        ;     ------------------------------  ------------------------------------           -----------------
        ;     (inner-id-or-proto . arg-spec)  cur-wrapped-body-list                          queued-arg-specs
        ;  -> inner-id-or-proto               (list #'(λ arg-spec . cur-wrapped-body-list))  (cons arg-spec queued-arg-specs)
        ;
        ; By the end, we've decomposed e.g.
        ;    (((innermost-id . args0) . args1) . args2)   orig-body-list
        ;    ^--validated--^  ^--unvalidated---------------------------^
        ; into
        ;    cur-prototype-stx      innermost-id
        ;    cur-wrapped-body-list  (list #'(λ args0 (λ args1 (λ args2 . orig-body-list)))))
        ;    queued-arg-specs       (list args0 args1 args2)
        ; at which point we validate all the queued arg specs and orig-body-list.
        ;
        ; One final note. Since `process-function-define` is only invoked for function defines,
        ; we keep the invariant that the loop is only entered for function defines, which means
        ; the check for a base case occurs right before the recursive call and not at the outside
        ; of the function.
        (define-values (loop)
          (lambda (cur-prototype-stx cur-wrapped-body-list queued-arg-specs)
            (define-values (inner-id-or-prototype) (stx-car cur-prototype-stx))
            (define-values (arg-spec) (stx-cdr cur-prototype-stx))
            (define-values (this-layer-rhs)
              (datum->syntax #f
                             (list* (quote-syntax lambda) arg-spec cur-wrapped-body-list)
                             full-stx))
            (if (identifier? inner-id-or-prototype)
                (begin (for-each (lambda (arg-spec)
                                   (check-procedure-arg-spec full-stx arg-spec))
                                 (cons arg-spec queued-arg-specs))
                       (raise-syntax-error-unless (stx-list? orig-body-stxlist)
                                                  "bad syntax (illegal use of `.' for procedure body)"
                                                  full-stx)
                       (raise-syntax-error-if (stx-null? orig-body-stxlist)
                                              "bad syntax (no expressions for procedure body)"
                                              full-stx)
                       (values inner-id-or-prototype this-layer-rhs))
                (if (stx-pair? inner-id-or-prototype)
                    (loop inner-id-or-prototype
                          (list this-layer-rhs)
                          (cons arg-spec queued-arg-specs))
                    (raise-syntax-error #f
                                        "bad syntax (not an identifier for procedure name, and not a nested procedure form)"
                                        full-stx
                                        inner-id-or-prototype)))))
        (loop orig-prototype-stx orig-body-stxlist null)))

    ; (process-identifier-define full-stx id body-stxlist)
    ;   full-stx     = #'(define id . body-stxlist)
    ;   id           : identifier?
    ;   body-stxlist : stx-list?
    ;  -> (values identifier? syntax?)
    ; Given the user-specified id and body for an identifier define. Validates
    ; and then returns the identifier and expression rhs for the final output
    ; define-values (or define-syntaxes).
    (define-values (process-identifier-define)
      (lambda (full-stx id body-stxlist)
        (raise-syntax-error-unless (stx-list? body-stxlist)
                                   "bad syntax (illegal use of `.')"
                                   full-stx)
        (raise-syntax-error-if (stx-null? body-stxlist)
                               "bad syntax (missing expression after identifier)"
                               full-stx)
        (raise-syntax-error-if (stx-pair? (stx-cdr body-stxlist))
                               "bad syntax (multiple expressions after identifier)"
                               full-stx)
        (values id (stx-car body-stxlist))))

    (define-values (process-define)
      (lambda (head-for-output-form stx)
        (raise-syntax-error-if (eq? (syntax-local-context) 'expression)
                               "not allowed in an expression context"
                               stx)
        ; Decompose (define id-or-function-prototype . body-stxlist)
        ; Errors detected here:
        ;   define     => "bad syntax"
        ;   (define)   => "bad syntax"
        (raise-syntax-error-unless (stx-pair? stx) "bad syntax" stx)
        (raise-syntax-error-unless (stx-pair? (stx-cdr stx)) "bad syntax" stx)
        (define-values (id-or-function-prototype) (stx-car (stx-cdr stx)))
        (define-values (body-stxlist) (stx-cdr (stx-cdr stx)))
        ; Dispatch on definition type to determine final identifier and rhs
        ; Errors detected here
        ;   (define gunk . _)  => "bad syntax (not an identifier or prototype of procedure to define)"
        (define-values (id rhs)
          (if (identifier? id-or-function-prototype)
              (process-identifier-define stx id-or-function-prototype body-stxlist)
              (if (stx-pair? id-or-function-prototype)
                  (process-function-define stx id-or-function-prototype body-stxlist)
                  (raise-syntax-error #f
                                      "bad syntax (not an identifier or prototype of procedure to define)"
                                      stx
                                      id-or-function-prototype))))
        ; Compose output
        (datum->syntax #f
                       (list head-for-output-form
                             (list id)
                             rhs)
                       stx))))

  (define-syntaxes (define)
    (lambda (stx)
      (process-define (quote-syntax define-values) stx)))

  (define-syntaxes (define-syntax)
    (lambda (stx)
      (process-define (quote-syntax define-syntaxes) stx)))

  (define-syntaxes (define-for-syntax)
    (lambda (stx)
      (datum->syntax #f
                     (list (quote-syntax begin-for-syntax)
                           (process-define (quote-syntax define-values) stx))
                     stx)))

  (define-syntaxes (-define)
    (make-rename-transformer (quote-syntax define)))

  (define-syntaxes (-define-syntax)
    (make-rename-transformer (quote-syntax define-syntax)))

  ; --------------------------------------------------
  ;
  ;
  ;           ;                            ;                  ;;;
  ;           ;                            ;                    ;
  ;           ;                           ;                     ;
  ;  ;  ;  ;  ; ;;;     ;;;   ; ;;;       ;   ;    ;  ; ;;;     ;       ;;;    ;;;;    ;;;;
  ;  ;  ;  ;  ;;   ;   ;   ;  ;;   ;     ;    ;    ;  ;;   ;    ;      ;   ;  ;    ;  ;    ;
  ;  ; ; ; ;  ;    ;  ;    ;  ;    ;    ;     ;    ;  ;    ;    ;     ;    ;  ;       ;
  ;  ; ; ; ;  ;    ;  ;;;;;;  ;    ;    ;     ;    ;  ;    ;    ;     ;;;;;;   ;;      ;;
  ;  ; ; ; ;  ;    ;  ;       ;    ;   ;      ;    ;  ;    ;    ;     ;          ;;      ;;
  ;  ; ; ; ;  ;    ;  ;       ;    ;   ;      ;    ;  ;    ;    ;     ;            ;       ;
  ;   ;   ;   ;    ;   ;      ;    ;  ;       ;   ;;  ;    ;    ;      ;      ;    ;  ;    ;
  ;   ;   ;   ;    ;    ;;;;  ;    ;  ;        ;;; ;  ;    ;    ;;;     ;;;;   ;;;;    ;;;;
  ;
  ;
  ; when and unless
  ;

  (define-syntaxes (when)
    (lambda (stx)
      (define-values (lst) (syntax->list stx))
      (raise-syntax-error-unless (pair? lst) "bad syntax" stx)
      (raise-syntax-error-if (null? (cdr lst)) "bad syntax (missing test expression and body)" stx)
      (raise-syntax-error-if (null? (cddr lst)) "bad syntax (missing body)" stx)
      (datum->syntax (quote-syntax here)
                     (list (quote-syntax if)
                           (cadr lst)
                           (list* (quote-syntax let-values)
                                  (quote-syntax ())
                                  (cddr lst))
                           (quote-syntax (void)))
                     stx)))

  (define-syntaxes (unless)
    (lambda (stx)
      (define-values (lst) (syntax->list stx))
      (raise-syntax-error-unless (pair? lst) "bad syntax" stx)
      (raise-syntax-error-if (null? (cdr lst)) "bad syntax (missing test expression and body)" stx)
      (raise-syntax-error-if (null? (cddr lst)) "bad syntax (missing body)" stx)
      (datum->syntax (quote-syntax here)
                     (list (quote-syntax if)
                           (cadr lst)
                           (quote-syntax (void))
                           (list* (quote-syntax let-values)
                                  (quote-syntax ())
                                  (cddr lst)))
                     stx)))

  ; --------------------------------------------------
  ;
  ;
  ;                   ;;;     ;;;          ;                          ;;;                          ;
  ;                     ;       ;          ;                            ;               ;          ;
  ;                     ;       ;         ;                             ;               ;         ;
  ;     ;;;;    ;;;;    ;       ;         ;     ;;;     ;;;;            ;       ;;;     ;         ;     ;;;     ;;;;
  ;    ;       ;   ;    ;       ;        ;     ;   ;   ;                ;      ;   ;  ;;;;;;     ;     ;   ;   ;
  ;   ;       ;    ;    ;       ;       ;     ;    ;  ;                 ;     ;    ;    ;       ;     ;    ;  ;
  ;   ;       ;    ;    ;       ;       ;     ;;;;;;  ;                 ;     ;;;;;;    ;       ;     ;;;;;;  ;
  ;   ;       ;    ;    ;       ;      ;      ;       ;                 ;     ;         ;      ;      ;       ;
  ;   ;       ;    ;    ;       ;      ;      ;       ;                 ;     ;         ;      ;      ;       ;
  ;    ;      ;   ;;    ;       ;     ;        ;       ;                ;      ;        ;     ;        ;       ;
  ;     ;;;;   ;;; ;    ;;;     ;;;   ;         ;;;;    ;;;;            ;;;     ;;;;     ;;;  ;         ;;;;    ;;;;
  ;
  ;
  ; call/ec and let/ec
  ;

  (define-values (call/ec) call-with-escape-continuation)

  (define-syntaxes (let/ec)
    (lambda (stx)
      (define-values (lst) (syntax->list stx))
      (raise-syntax-error-unless (pair? lst) "bad syntax" stx)
      (define-values (len) (length lst))
      (raise-syntax-error-if (= len 1) "bad syntax (missing identifier and body)" stx)
      (raise-syntax-error-if (= len 2) "bad syntax (missing body)" stx)
      (datum->syntax (quote-syntax here)
                     (list (quote-syntax call-with-escape-continuation)
                           (datum->syntax #f
                                          (list* (quote-syntax lambda)
                                                 (list (cadr lst))
                                                 (stx-cdr (stx-cdr stx)))
                                          stx))
                     stx)))

  ;
  ; --------------------------------------------------
  )
