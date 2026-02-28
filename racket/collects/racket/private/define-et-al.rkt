
;;----------------------------------------------------------------------
;; -define, when, unless, let/ec, define-struct

(module define-et-al '#%kernel
  (#%require (for-syntax '#%kernel "stx.rkt" "qq-and-or.rkt" 
                         "cond.rkt"))

  (#%provide -define -define-syntax
             define define-syntax define-for-syntax define-values-for-syntax
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
  
  (define-syntaxes (define-values-for-syntax)
    (lambda (stx)
      (define-values (lst) (syntax->list stx))
      (raise-syntax-error-unless lst "bad syntax (illegal use of `.')" stx)
      (if (= 3 (length lst))
          (void)
          (begin (raise-syntax-error-if (= 1 (length lst)) "bad syntax (missing names to define)" stx)
                 (raise-syntax-error-if (= 2 (length lst)) "bad syntax (missing expression after identifiers)" stx)
                 (raise-syntax-error #f "bad syntax (multiple expressions after identifiers)" stx)))
      (define-values (ids) (syntax->list (cadr lst)))
      (raise-syntax-error-unless (if ids (andmap identifier? ids) #f)
                                 "bad syntax (expected a list of identifiers)"
                                 stx
                                 (cadr lst))
      (datum->syntax #f
                     (list (quote-syntax begin-for-syntax)
                           (datum->syntax #f
                                          (list* (quote-syntax define-values) (cdr lst))
                                          stx))
                     stx)))

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
            (begin (check-procedure-arg-name (stx-car arg-spec))
                   (check-procedure-arg-spec (stx-cdr arg-spec)))
            (check-procedure-arg-name (stx-car arg-spec)))))

    ; (process-function-define full-stx prototype-stx body-list)
    ;   full-stx       = #'(define original-function-prototype . orig-body-list)
    ;   prototype-stx  = #'(inner-id-or-function-prototype . arg-spec) 
    ;   body-list      : (listof syntax?)  ; expressions for function body
    (define-values (process-function-define)
      (lambda (full-stx prototype-stx body-list)  ; invariant: head != identifier
        (raise-syntax-error-unless (stx-pair? prototype-stx)
                                   "bad syntax (not an identifier for procedure name, and not a nested procedure form)"
                                   full-stx
                                   prototype-stx)
        (define-values (inner-id-or-function-prototype) (stx-car prototype-stx))
        (define-values (arg-spec) (stx-cdr prototype-stx))
        (define-values (this-layer-rhs)
          (datum->syntax #f
                         (list* (quote-syntax lambda) arg-spec body-list)
                         full-stx))
        (if (identifier? inner-id-or-function-prototype)
            (values inner-id-or-function-prototype this-layer-rhs)
            (process-function-define full-stx inner-id-or-function-prototype (list this-layer-rhs)))))

    (define-values (process-define)
      (lambda (head-for-output-form stx)
        ; Decompose (define id-or-proto . body-list)
        (define-values (lst) (syntax->list stx))
        (raise-syntax-error-unless lst "bad syntax (illegal use of `.')" stx)
        (raise-syntax-error-unless (<= 2 (length lst)) "bad syntax" stx)
        (define-values (id-or-function-prototype) (cadr lst))
        (define-values (body-list) (cddr lst))
        ; Iteratively   (inner-id-or-proto . args)  body-list
        ;            -> inner-id-or-proto           (lambda args body-list)
        ; Base case:    id                          rhs
        (define-values (id rhs)
          (if (identifier? id-or-function-prototype)
              (begin (raise-syntax-error-if (null? body-list)
                                            "bad syntax (missing expression after identifier)"
                                            stx)
                     (raise-syntax-error-if (not (null? (cdr body-list)))
                                            "bad syntax (multiple expressions after identifier)"
                                            stx)
                     (values id-or-function-prototype (car body-list)))
              (begin (raise-syntax-error-if (null? body-list)
                                            "bad syntax (no expressions for procedure body)"
                                            stx)
                     (process-function-define stx id-or-function-prototype body-list))))
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
      (raise-syntax-error-unless (pair? lst) "bad syntax (illegal use of `.')" stx)
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
      (raise-syntax-error-unless (pair? lst) "bad syntax (illegal use of `.')" stx)
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
      (raise-syntax-error-unless (pair? lst) "bad syntax (illegal use of `.')" stx)
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
