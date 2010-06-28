#reader scribble/reader
#lang scheme/base
(require scribblings/htdp-langs/common
         scribble/decode
         (for-label deinprogramm/DMdA-beginner))

(provide prim-nonterms
         schemegrammar*-DMdA)

(define ex-str "Dies ist eine Zeichenkette, die \" enthält.")

(define-syntax-rule (schemegrammar*-DMdA
                     #:literals (lit ...)
		     (def-rule ...)
		     (prod ...)
                     (expr-rule ...))
  (schemegrammar*
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
   [definition @#,scheme[(define id expr)]
     @#,scheme[(define-record-procedures id id id (id (... ...)))]
     @#,scheme[(define-record-procedures-parametric (id id (... ...)) id id (id (... ...)))]
     @#,scheme[(: id sig)]
     def-rule ...]
   prod ...
   [expr @#,scheme[(code:line (expr expr (... ...)) (code:comment @#,seclink["application"]{Prozedurapplikation}))]
	 @#,scheme[#t]
	 @#,scheme[#f]
	 @#,scheme[number]
	 @#,scheme[string]
	 @#,scheme[(lambda (id (... ...)) expr)]
	 @#,scheme[(code:line id (code:comment @#,seclink["id"]{Bezeichner}))]
	 @#,scheme[(cond (expr expr) (expr expr) (... ...))]
	 @#,scheme[(cond (expr expr) (... ...) (else expr))]
	 @#,scheme[(if expr expr)]
	 @#,scheme[(and expr (... ...))]
	 @#,scheme[(or expr (... ...))]
	 @#,scheme[(let ((id expr) (... ...)) expr)]
	 @#,scheme[(letrec ((id expr) (... ...)) expr)]
	 @#,scheme[(let* ((id expr) (... ...)) expr) ]
	 @#,scheme[(begin expr expr (... ...))]
	 @#,scheme[(signature sig)]
	 @#,scheme[(for-all ((id sig) (... ...)) expr)]
	 @#,scheme[(==> expr expr)]
	 expr-rule ...]
   [sig  id
	      @#,scheme[(predicate expr)]
	      @#,scheme[(one-of expr (... ...))]
	      @#,scheme[(mixed sig (... ...))]
	      @#,scheme[(code:line (sig (... ...) -> sig) (code:comment @#,seclink["proc-signature"]{Prozedur-Signatur}))]
	      @#,scheme[(list sig)]
	      @#,scheme[(code:line %a %b %c (code:comment @#,seclink["signature-variable"]{Signatur-Variable}))]
	      @#,scheme[(combined sig (... ...))]
	      @#,scheme[signature]
   ]
   [test-case @#,scheme[(check-expect expr expr)]
              @#,scheme[(check-within expr expr expr)]
	      @#,scheme[(check-member-of expr expr (... ...))]
	      @#,scheme[(check-range expr expr expr)]
              @#,scheme[(check-error expr expr)]
	      @#,scheme[(check-property expr)]]
   #;(...
    [library-require @#,scheme[(require string)]
		     @#,scheme[(require module-id)]
                     @#,scheme[(require (lib string string ...))]
                     @#,scheme[(require (planet string package))]])
   #;(...
    [package @#,scheme[(string string number number)]])))

(define prim-nonterms
  (make-splice
   (list

@t{Ein @scheme[_id] ist eine Folge von Zeichen, die weder Leerzeichen
noch eins der folgenden Zeichen enthält:}

@t{@hspace[2] @litchar{"} @litchar{,} @litchar{'} @litchar{`} 
@litchar{(} @litchar{)} @litchar{[} @litchar{]} 
@litchar["{"] @litchar["}"] @litchar{|} @litchar{;}
@litchar{#}}

@t{Ein @scheme[_number] ist eine Zahl wie z.B. @scheme[123], @scheme[3/2] oder
@scheme[5.5].}

@t{Ein @scheme[_string] ist eine Zeichenkette, und durch ein Paar von @litchar{"} umschlossen. 
So sind z.B. @scheme["abcdef"],
@scheme["This is a string"] und @scheme[#,ex-str] Zeichenketten.}
)))
