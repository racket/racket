
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

  
  ;; No error checking here, because these macros merely help
  ;;  us write macros before the real define and define-syntax
  (define-syntaxes (-define -define-syntax)
    (let ([here (quote-syntax here)])
      (let ([mk-define
	     (lambda (base)
	       (lambda (code)
		 (let ([body (stx-cdr code)])
		   (let ([first (stx-car body)])
		     (cond
		      [(identifier? first)
		       (datum->syntax
			here
			`(,base (,first) ,@(stx->list (stx-cdr body)))
			code)]
		      [else
		       (let ([pbody (stx-cdr body)])
			 (datum->syntax
			  (quote-syntax here)
			  `(,base (,(stx-car first)) 
				  (lambda ,(stx-cdr first) ,@(stx->list pbody)))
			  code))])))))])
	(values (mk-define (quote-syntax define-values))
		(mk-define (quote-syntax define-syntaxes))))))

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
