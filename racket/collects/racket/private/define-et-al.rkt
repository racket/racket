
;;----------------------------------------------------------------------
;; -define, when, unless, let/ec, define-struct

(module define-et-al '#%kernel
  (#%require (for-syntax '#%kernel "stx.rkt"))

  (#%provide define define-syntax define-for-syntax
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


  ; define, define-syntax, and define-for-syntax
  ;
  ; These are not user-visible (they get replaced by the full versions which
  ; use the norm-define.rkt machinery and support keywords) and so we put
  ; minimal effort into having user-friendly error reporting.
  (define-syntaxes (define define-syntax define-for-syntax)
    (let-values ()
      (define-values (process-define*)
        (lambda (full-stx id-or-prototype body-list)
          (if (identifier? id-or-prototype)
              (values id-or-prototype body-list)
              (if (stx-pair? id-or-prototype)
                  (let-values ([(nested) (stx-car id-or-prototype)]
                               [(arg-spec) (stx-cdr id-or-prototype)])
                    (process-define* full-stx
                                     nested
                                     (list (datum->syntax #f
                                                          (list* (quote-syntax lambda)
                                                                 arg-spec
                                                                 body-list)
                                                          full-stx))))
                  (raise-syntax-error #f "bad syntax" full-stx id-or-prototype)))))

      (define-values (process-define)
        (lambda (head-for-output-form stx)
          (define-values (lst) (syntax->list stx))
          (raise-syntax-error-unless lst "bad syntax" stx)
          (raise-syntax-error-unless (pair? (cdr lst)) "bad syntax" stx)
          (define-values (id rhs-list)
            (process-define* stx (cadr lst) (cddr lst)))
          (datum->syntax #f
                         (list* head-for-output-form (list id) rhs-list)
                         stx)))

    (values
     (lambda (stx)
       (process-define (quote-syntax define-values) stx))
     (lambda (stx)
      (process-define (quote-syntax define-syntaxes) stx))
     (lambda (stx)
      (datum->syntax #f
                     (list (quote-syntax begin-for-syntax)
                           (process-define (quote-syntax define-values) stx))
                     stx)))))

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
