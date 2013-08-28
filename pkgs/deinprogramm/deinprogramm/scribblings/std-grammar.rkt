#reader scribble/reader
#lang scheme/base
(require scribblings/htdp-langs/common
         scribble/decode
         (for-label deinprogramm/DMdA-beginner))

(provide prim-nonterms
         racketgrammar*-DMdA)

(define ex-str "Dies ist eine Zeichenkette, die \" enthält.")

(define-syntax-rule (racketgrammar*-DMdA
                     #:literals (lit ...)
                     (def-rule ...)
                     (prod ...)
                     (expr-rule ...))
  (racketgrammar*
   #:literals (define define-record-procedures lambda cond if and or let letrec let* begin 
		#;require lib planet
		check-expect check-within check-error
		signature :
		predicate one-of mixed list %a %b %c
		lit ...)
   (... [program (code:line def-or-expr ...)])
   [def-or-expr definition
     expr
     test-case
     #;library-require]
   [definition @#,racket[(define id expr)]
     @#,racket[(define-record-procedures id id id (id (... ...)))]
     @#,racket[(define-record-procedures-parametric (id id (... ...)) id id (id (... ...)))]
     @#,racket[(: id sig)]
     def-rule ...]
   prod ...
   [expr @#,racket[(code:line (expr expr (... ...)) (code:comment @#,seclink["application"]{Prozedurapplikation}))]
	 @#,racket[#t]
	 @#,racket[#f]
	 @#,racket[number]
	 @#,racket[string]
	 @#,racket[(lambda (id (... ...)) expr)]
	 @#,racket[(code:line id (code:comment @#,seclink["id"]{Bezeichner}))]
	 @#,racket[(cond (expr expr) (expr expr) (... ...))]
	 @#,racket[(cond (expr expr) (... ...) (else expr))]
	 @#,racket[(if expr expr)]
	 @#,racket[(and expr (... ...))]
	 @#,racket[(or expr (... ...))]
	 @#,racket[(let ((id expr) (... ...)) expr)]
	 @#,racket[(letrec ((id expr) (... ...)) expr)]
	 @#,racket[(let* ((id expr) (... ...)) expr) ]
	 @#,racket[(begin expr expr (... ...))]
	 @#,racket[(signature sig)]
	 @#,racket[(for-all ((id sig) (... ...)) expr)]
	 @#,racket[(==> expr expr)]
	 expr-rule ...]
   [sig  id
	      @#,racket[(predicate expr)]
	      @#,racket[(one-of expr (... ...))]
	      @#,racket[(mixed sig (... ...))]
	      @#,racket[(code:line (sig (... ...) -> sig) (code:comment @#,seclink["proc-signature"]{Prozedur-Signatur}))]
	      @#,racket[(list sig)]
	      @#,racket[(code:line %a %b %c (code:comment @#,seclink["signature-variable"]{Signatur-Variable}))]
	      @#,racket[(combined sig (... ...))]
	      @#,racket[signature]
   ]
   [test-case @#,racket[(check-expect expr expr)]
              @#,racket[(check-within expr expr expr)]
	      @#,racket[(check-member-of expr expr (... ...))]
	      @#,racket[(check-range expr expr expr)]
              @#,racket[(check-error expr expr)]
	      @#,racket[(check-property expr)]]
   #;(...
    [library-require @#,racket[(require string)]
		     @#,racket[(require module-id)]
                     @#,racket[(require (lib string string ...))]
                     @#,racket[(require (planet string package))]])
   #;(...
    [package @#,racket[(string string number number)]])))

(define prim-nonterms
  (make-splice
   (list

@t{Ein @racket[_id] ist eine Folge von Zeichen, die weder Leerzeichen
noch eins der folgenden Zeichen enthält:}

@t{@hspace[2] @litchar{"} @litchar{,} @litchar{'} @litchar{`} 
@litchar{(} @litchar{)} @litchar{[} @litchar{]} 
@litchar["{"] @litchar["}"] @litchar{|} @litchar{;}
@litchar{#}}

@t{Ein @racket[_number] ist eine Zahl wie z.B. @racket[123], @racket[3/2] oder
@racket[5.5].}

@t{Ein @racket[_string] ist eine Zeichenkette, und durch ein Paar von @litchar{"} umschlossen. 
So sind z.B. @racket["abcdef"],
@racket["This is a string"] und @racket[#,ex-str] Zeichenketten.}
)))
