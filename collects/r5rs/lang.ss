
(module lang mzscheme

  ;; values
  (provide car cdr caar cadr cdar cddr
	   caaar caadr cadar caddr cdaar cdadr cddar cdddr
	   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	   map = < > <= >= max min + - * / 
	   abs gcd lcm exp log sin cos tan not eq?
	   call-with-current-continuation make-string
	   symbol->string string->symbol make-rectangular 
	   exact->inexact inexact->exact number->string string->number 
	   rationalize output-port? current-input-port current-output-port current-error-port 
	   open-input-file open-output-file close-input-port close-output-port
	   with-output-to-file transcript-on transcript-off flush-output
	   string-length string-ci<=? string-ci>=? string-append 
	   string->list list->string string-fill! 
	   vector-length vector->list list->vector vector-fill!
	   char-alphabetic? char-numeric? char-whitespace? 
	   char-upper-case? char-lower-case? char->integer integer->char char-downcase
	   call-with-output-file call-with-input-file with-input-from-file
	   apply for-each symbol? pair? cons set-car! set-cdr! null? list? list length append reverse
	   list-tail list-ref memq memv member assq assv assoc procedure?
	   number? complex? real? rational? integer? exact? inexact? zero?
	   positive?  negative? odd? even? 
	   quotient remainder modulo floor ceiling truncate round 
	   numerator denominator asin acos atan sqrt
	   expt make-polar real-part imag-part angle magnitude input-port?
	   read read-char peek-char eof-object?
	   char-ready? write display newline write-char load 
	   string? string string-ref string-set! string=? substring string-copy
	   string-ci=? string<? string>? string<=? string>=? string-ci<? string-ci>?
	   vector? make-vector vector vector-ref vector-set! 
	   char? char=? char<? char>? char<=? char>=? 
	   char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? 
	   char-upcase boolean? eqv? equal? force
	   call-with-values values eval port? scheme-report-environment null-environment 
	   interaction-environment dynamic-wind)

  ;; Copied from R5rS:
  (define undefined (letrec ([u u]) u))
  (define-syntax r5rs:letrec
    (syntax-rules ()
      ((r5rs:letrec ((var1 init1) ...) body ...)
       (r5rs:letrec "generate_temp_names"
	 (var1 ...)
	 ()
	 ((var1 init1) ...)
	 body ...))
      ((r5rs:letrec "generate_temp_names"
	 ()
	 (temp1 ...)
	 ((var1 init1) ...)
	 body ...)
       (let ((var1 undefined) ...)
	 (let ((temp1 init1) ...)
	   (set! var1 temp1)
	   ...
	   (let ()
	     body ...))))
      ((r5rs:letrec "generate_temp_names"
	 (x y ...)
	 (temp ...)
	 ((var1 init1) ...)
	 body ...)
       (r5rs:letrec "generate_temp_names"
	 (y ...)
	 (newtemp temp ...)
	 ((var1 init1) ...)
	 body ...))))

  ;; redefine to avoid curried define forms
  (define-syntax (r5rs:define stx)
    (syntax-case stx ()
      [(_ id expr) (identifier? #'id)
       #'(define id expr)]
      [(_ (id . args) . body) (identifier? #'id)
       #'(define (id . args) . body)]))

  ;; syntax
  (provide quasiquote unquote unquote-splicing
	   if let and or cond case delay do
	   (rename r5rs:letrec letrec) (rename r5rs:define define)
	   let* begin lambda quote set!
	   define-syntax let-syntax letrec-syntax

	   ;; We have to include the following MzScheme-isms to do anything,
	   ;; but they're not legal R5RS names, anyway.
	   #%app #%datum #%top #%top-interaction
	   (rename synrule-in-stx-module-begin #%module-begin)
	   (rename require #%require)
	   (rename provide #%provide))

  (provide-for-syntax syntax-rules)
  
  (define-syntax synrule-in-stx-module-begin
    (lambda (stx)
      (datum->syntax-object
       (quote-syntax here)
       (list* (quote-syntax #%plain-module-begin)
	      (list 'require-for-syntax 
		    (datum->syntax-object
		     stx
		     '(only mzscheme syntax-rules)))
	      (cdr (syntax-e stx)))
       stx))))
