
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

  (-define-syntax let/ec 
    (lambda (code)
      (let ([l (syntax->list code)])
	(if (and l
		 (> (length l) 2)
		 (identifier? (cadr l)))
	    (let ([var (cadr l)]
		  [exprs (stx-cdr (stx-cdr code))])
	      (datum->syntax
	       (quote-syntax here)
	       `(call/ec (lambda (,var) ,@(stx->list exprs)))
	       code))
	    (raise-syntax-error
	     #f
	     "bad syntax"
	     code)))))

  ;
  ; --------------------------------------------------
  )
