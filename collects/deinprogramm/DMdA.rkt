#lang scheme/base

(require syntax/docprovide)

(require test-engine/scheme-tests
	 (lib "test-info.scm" "test-engine")
	 test-engine/scheme-tests
	 scheme/class)

(require deinprogramm/signature/module-begin
	 (except-in deinprogramm/signature/signature signature-violation)
	 (except-in deinprogramm/signature/signature-syntax property))

(require (for-syntax scheme/base)
	 (for-syntax stepper/private/syntax-property))

(require deinprogramm/define-record-procedures)

(require (only-in lang/private/teachprims define-teach teach-equal? beginner-equal~?))

(require (for-syntax deinprogramm/syntax-checkers))

(require (rename-in deinprogramm/quickcheck/quickcheck
		    (property quickcheck:property)))

(provide provide lib planet rename-out require #%datum #%module-begin #%top-interaction) ; so we can use this as a language

(provide (all-from-out deinprogramm/define-record-procedures))
(provide (all-from-out test-engine/scheme-tests))
(provide signature define-contract :
	 contract ; legacy
	 -> mixed one-of predicate combined list-of)

(provide number real rational integer natural
	 boolean true false
	 string symbol
	 empty-list
	 unspecific
	 any
	 property)

(define-syntax provide/rename
  (syntax-rules ()
    ((provide/rename (here there) ...)
     (begin
       (provide (rename-out (here there))) ...))))

(provide/rename
 (DMdA-define define)
 (DMdA-let let)
 (DMdA-let* let*)
 (DMdA-letrec letrec)
 (DMdA-lambda lambda)
 (DMdA-cond cond)
 (DMdA-if if)
 (DMdA-else else)
 (DMdA-begin begin)
 (DMdA-and and)
 (DMdA-or or)
 (DMdA-dots ..)
 (DMdA-dots ...)
 (DMdA-dots ....)
 (DMdA-dots .....)
 (DMdA-dots ......)
 (DMdA-app #%app)
 (DMdA-top #%top)
 (DMdA-set! set!)
 (module-begin DMdA-module-begin))

(provide DMdA-advanced-lambda
	 DMdA-advanced-define)

(provide for-all ==>
	 check-property
	 expect expect-within expect-member-of expect-range)

(provide quote)

(provide-and-document
 procedures
 ("Zahlen"
  (number? (any -> boolean)
	   "feststellen, ob ein Wert eine Zahl ist")

  (= (number number number ... -> boolean)
     "Zahlen auf Gleichheit testen")
  (< (real real real ... -> boolean)
     "Zahlen auf kleiner-als testen")
  (> (real real real ... -> boolean)
     "Zahlen auf größer-als testen")
  (<= (real real real ... -> boolean)
      "Zahlen auf kleiner-gleich testen")
  (>= (real real real ... -> boolean)
      "Zahlen auf größer-gleich testen")
  
  (+ (number number number ... -> number)
     "Summe berechnen")
  (- (number number ... -> number)
     "bei mehr als einem Argument Differenz zwischen der ersten und der Summe aller weiteren Argumente berechnen; bei einem Argument Zahl negieren")
  (* (number number number ... -> number)
     "Produkt berechnen")
  (/ (number number number ... -> number)
     "das erste Argument durch das Produkt aller weiteren Argumente berechnen")
  (max (real real ... -> real)
       "Maximum berechnen")
  (min (real real ... -> real)
       "Minimum berechnen")
  (quotient (integer integer -> integer)
	    "ganzzahlig dividieren")
  (remainder (integer integer -> integer)
	     "Divisionsrest berechnen")
  (modulo (integer integer -> integer)
	  "Divisionsmodulo berechnen")
  (sqrt (number -> number)
	"Quadratwurzel berechnen")
  (expt (number number -> number)
	"Potenz berechnen (erstes Argument hoch zweites Argument)")
  (abs (real -> real)
       "Absolutwert berechnen")
  
  ;; fancy numeric 
  (exp (number -> number)
       "Exponentialfunktion berechnen (e hoch Argument)")
  (log (number -> number)
       "natürlichen Logarithmus (Basis e) berechnen")
  
  ;; trigonometry
  (sin (number -> number)
       "Sinus berechnen (Argument in Radian)")
  (cos (number -> number)
       "Cosinus berechnen (Argument in Radian)")
  (tan (number -> number)
       "Tangens berechnen (Argument in Radian)")
  (asin (number -> number)
	"Arcussinus berechnen (in Radian)")
  (acos (number -> number)
	"Arcuscosinus berechnen (in Radian)")
  (atan (number -> number)
	"Arcustangens berechnen (in Radian)")
  
  (exact? (number -> boolean)
	  "feststellen, ob eine Zahl exakt ist")
  
  (integer? (any -> boolean)
	    "feststellen, ob ein Wert eine ganze Zahl ist")
  (natural? (any -> boolean)
	    "feststellen, ob ein Wert eine natürliche Zahl (inkl. 0) ist")
  
  (zero? (number -> boolean)
	 "feststellen, ob eine Zahl Null ist") 
  (positive? (number -> boolean)
	     "feststellen, ob eine Zahl positiv ist")
  (negative? (number -> boolean)
	     "feststellen, ob eine Zahl negativ ist")
  (odd? (integer -> boolean)
	"feststellen, ob eine Zahl ungerade ist")
  (even? (integer -> boolean)
	 "feststellen, ob eine Zahl gerade ist")

  (lcm (integer integer ... -> natural)
       "kleinstes gemeinsames Vielfaches berechnen")
  
  (gcd (integer integer ... -> natural)
       "größten gemeinsamen Teiler berechnen")
  
  (rational? (any -> boolean)
	     "feststellen, ob eine Zahl rational ist")
  
  (numerator (rational -> integer)
	     "Zähler eines Bruchs berechnen")
  
  (denominator (rational -> natural)
	       "Nenner eines Bruchs berechnen")
  
  (inexact? (number -> boolean)
	    "feststellen, ob eine Zahl inexakt ist")
  
  (real? (any -> boolean)
	 "feststellen, ob ein Wert eine reelle Zahl ist")
  
  (floor (real -> integer)
	 "nächste ganze Zahl unterhalb einer rellen Zahlen berechnen")
  
  (ceiling (real -> integer)
	   "nächste ganze Zahl oberhalb einer rellen Zahlen berechnen")
  
  (round (real -> integer)
	 "relle Zahl auf eine ganze Zahl runden")
  
  (complex? (any -> boolean)
	    "feststellen, ob ein Wert eine komplexe Zahl ist")
  
  (make-polar (real real -> number)
	      "komplexe Zahl aus Abstand zum Ursprung und Winkel berechnen")
  
  (real-part (number -> real)
	     "reellen Anteil einer komplexen Zahl extrahieren")
  
  (imag-part (number -> real)
	     "imaginären Anteil einer komplexen Zahl extrahieren")
  
  (magnitude (number -> real)
	     "Abstand zum Ursprung einer komplexen Zahl berechnen")
  
  (angle (number -> real)
	 "Winkel einer komplexen Zahl berechnen")
  
  (exact->inexact (number -> number)
		  "eine Zahl durch eine inexakte Zahl annähern")
  
  (inexact->exact (number -> number)
		  "eine Zahl durch eine exakte Zahl annähern")
  
  ;;    "Odds and ends"
  
  (number->string (number -> string)
		  "Zahl in Zeichenkette umwandeln")

  (string->number (string -> (mixed number false))
		  "Zeichenkette in Zahl umwandeln, falls möglich")
  
  (random (natural -> natural)
	  "eine natürliche Zufallszahl berechnen, die kleiner als das Argument ist")
  
  (current-seconds (-> natural)
		   "aktuelle Zeit in Sekunden seit einem unspezifizierten Startzeitpunkt berechnen"))

 ("boolesche Werte" 
  (boolean? (any -> boolean)
	    "feststellen, ob ein Wert ein boolescher Wert ist")
  
  ((DMdA-not not) (boolean -> boolean)
   "booleschen Wert negieren")

  (boolean=? (boolean boolean -> boolean)
	     "Booleans auf Gleichheit testen")

  (true? (any -> boolean)
	 "feststellen, ob ein Wert #t ist")
  (false? (any -> boolean)
	  "feststellen, ob ein Wert #f ist"))

 ("Listen"
  (empty list "die leere Liste")
  (make-pair (%a (list-of %a) -> (list-of %a))
	     "erzeuge ein Paar aus Element und Liste")
  ((DMdA-cons cons) (%a (list-of %a) -> (list-of %a))
	     "erzeuge ein Paar aus Element und Liste")
  (pair? (any -> boolean)
	 "feststellen, ob ein Wert ein Paar ist")	
  (cons? (any -> boolean)
	 "feststellen, ob ein Wert ein Paar ist")
  (empty? (any -> boolean)
	  "feststellen, ob ein Wert die leere Liste ist")
  
  (first ((list-of %a) -> %a)
	 "erstes Element eines Paars extrahieren")
  (rest ((list-of %a) -> (list-of %a))
	"Rest eines Paars extrahieren")

  (list (%a ... -> (list-of %a))
	"Liste aus den Argumenten konstruieren")

  (length ((list-of %a) -> natural)
	  "Länge einer Liste berechnen")

  (fold (%b (%a %b -> %b) (list-of %a) -> %b)
	 "Liste einfalten.")
  
  ((DMdA-append append) ((list-of %a) ... -> (list-of %a))
   "mehrere Listen aneinanderhängen")

  (list-ref ((list-of %a) natural -> %a)
	    "das Listenelement an der gegebenen Position extrahieren")
  
  (reverse ((list-of %a)  -> (list-of %a))
	   "Liste in umgekehrte Reihenfolge bringen"))

 ("Computer"
  (computer signature
	    "Signatur für Computer")
  (make-computer (string rational rational -> computer)
		 "Computer aus Prozessorname, Arbeitsspeicher und Festplattenkapazität konstruieren")
  (computer? (any -> boolean)
	     "feststellen, ob Wert ein Computer ist")
  (computer-processor (computer -> string)
		      "Prozessorname aus Computer extrahieren")
  (computer-ram (computer -> rational)
		"Arbeitsspeicher aus Computer extrahieren")
  (computer-hard-drive (computer -> rational)
		       "Festplattenkapazität aus Computer extrahieren"))

 ("Schokokekse"
  (chocolate-cookie signature
		    "Signatur für Schokokekse")
  (make-chocolate-cookie (number number -> chocolate-cookie)
			 "Schokokeks aus Schoko- und Keks-Anteil konstruieren")
  (chocolate-cookie? (any -> boolean)
		     "feststellen, ob ein Wert ein Schokokeks ist")
  (chocolate-cookie-chocolate (chocolate-cookie -> number)
			      "Schoko-Anteil eines Schokokekses extrahieren")
  (chocolate-cookie-cookie (chocolate-cookie -> number)
			   "Keks-Anteil eines Schokokekses extrahieren"))

 ;; #### Zeichen sollten noch dazu, Vektoren wahrscheinlich auch

 ("Zeichenketten"
  (string? (any -> boolean)
	   "feststellen, ob ein Wert eine Zeichenkette ist")

  (string=? (string string string ... -> boolean)
	    "Zeichenketten auf Gleichheit testen")
  (string<? (string string string ... -> boolean)
	    "Zeichenketten lexikografisch auf kleiner-als testen")
  (string>? (string string string ... -> boolean)
	    "Zeichenketten lexikografisch auf größer-als testen")
  (string<=? (string string string ... -> boolean)
	     "Zeichenketten lexikografisch auf kleiner-gleich testen")
  (string>=? (string string string ... -> boolean)
	     "Zeichenketten lexikografisch auf größer-gleich testen")

  (string-append (string string ... -> string)
		 "Hängt Zeichenketten zu einer Zeichenkette zusammen")

  (strings-list->string ((list string) -> string)
			"Eine Liste von Zeichenketten in eine Zeichenkette umwandeln")

  (string->strings-list (string -> (list string))
			"Eine Zeichenkette in eine Liste von Zeichenketten mit einzelnen Zeichen umwandeln")

  (string-length (string -> natural)
		 "Liefert Länge einer Zeichenkette"))

 ("Symbole"
  (symbol? (any -> boolean)
	   "feststellen, ob ein Wert ein Symbol ist")
  (symbol->string (symbol -> string)
		  "Symbol in Zeichenkette umwandeln")
  (string->symbol (string -> symbol)
		  "Zeichenkette in Symbol umwandeln"))
 
 ("Verschiedenes"
  (equal? (%a %b -> boolean)
	  "zwei Werte auf Gleichheit testen")
  (eq? (%a %b -> boolean)
       "zwei Werte auf Selbheit testen")
  ((DMdA-write-string write-string) (string -> unspecific)
   "Zeichenkette in REPL ausgeben")
  (write-newline (-> unspecific)
		 "Zeilenumbruch ausgeben")
  (violation (string -> unspecific)
	     "Programmm mit Fehlermeldung abbrechen")

  (map ((%a -> %b) (list %a) -> (list %b))
       "Prozedur auf alle Elemente einer Liste anwenden, Liste der Resultate berechnen")
  (for-each ((%a -> %b) (list %a) -> unspecific)
	    "Prozedur von vorn nach hinten auf alle Elemente einer Liste anwenden")
  (apply (procedure (list %a) -> %b)
	 "Prozedur auf Liste ihrer Argumente anwenden")
  (read (-> any)
	"Externe Repräsentation eines Werts in der REPL einlesen und den zugehörigen Wert liefern")))

(define (make-pair f r)
  (when (and (not (null? r))
	     (not (pair? r)))
    (raise
     (make-exn:fail:contract
      (string->immutable-string
       (format "Zweites Argument zu make-pair ist keine Liste, sondern ~e" r))
      (current-continuation-marks))))
  (cons f r))

(define (first l)
  (when (not (pair? l))
    (raise
     (make-exn:fail:contract
      (string->immutable-string
       (format "Argument zu first kein Paar, sondern ~e" l))
      (current-continuation-marks))))
  (car l))

(define (rest l)
  (when (not (pair? l))
    (raise
     (make-exn:fail:contract
      (string->immutable-string
       (format "Argument zu rest kein Paar, sondern ~e" l))
      (current-continuation-marks))))
  (cdr l))

(define empty '())

(define (empty? obj)
  (null? obj))

(define (cons? obj)
  (pair? obj))

(define-teach DMdA cons
  (lambda (f r)
    (make-pair f r)))

(define-teach DMdA append
  (lambda args
    (let loop ((args args)
	       (seen-rev '()))
      (when (not (null? args))
	(let ((arg (car args)))
	  (when (and (not (null? arg))
		     (not (pair? arg)))
	    (raise
	     (make-exn:fail:contract
	      (string->immutable-string
	       (format "Argument zu append keine Liste, sondern ~e; restliche Argumente:~a"
		       arg
		       (apply string-append
			      (map (lambda (arg)
				     (format " ~e" arg))
				   (append (reverse seen-rev)
					   (list '<...>)
					   (cdr args))))))
	      (current-continuation-marks))))
	  (loop (cdr args)
		(cons arg seen-rev)))))
  
  (apply append args)))

(define fold
  (lambda (unit combine lis)
    (cond
      ((empty? lis) unit)
      ((pair? lis) 
       (combine (first lis)
                (fold unit combine (rest lis))))
      (else
       (raise
	(make-exn:fail:contract
	 (string->immutable-string
	  (format "Argument zu fold keine Liste, sondern ~e; andere Argumente: ~e ~e"
		  lis
		  unit combine))
	 (current-continuation-marks)))))))

;; This is copied from collects/lang/private/beginner-funs.rkt
;; Test-suite support (require is really an effect
;;  to make sure that it's loaded)
(require "test-suite.rkt")

(define-for-syntax (binding-in-this-module? b)
  (and (list? b)
       (module-path-index? (car b))
       (let-values (((path base) (module-path-index-split (car b))))
	 (and (not path) (not base)))))

(define-for-syntax (transform-DMdA-define stx mutable?)
  (unless (memq (syntax-local-context) '(module top-level))
    (raise-syntax-error
     #f "Define muss ganz außen stehen" stx))
  (syntax-case stx ()
    ((DMdA-define)
     (raise-syntax-error
      #f "Definition ohne Operanden" stx))
    ((DMdA-define v)
     (raise-syntax-error
      #f "Define erwartet zwei Operanden, nicht einen" stx))
    ((DMdA-define var expr)
     (begin
       (check-for-id!
	(syntax var)
	"Der erste Operand der Definition ist kein Bezeichner")
       
       (let ((binding (identifier-binding (syntax var))))
	 (when binding
	   (if (binding-in-this-module? binding)
	       (raise-syntax-error
		#f
		"Zweite Definition für denselben Namen"
		stx)
	       (raise-syntax-error
		#f
		"Dieser Name gehört einer eingebauten Prozedur und kann nicht erneut definiert werden" (syntax var)))))
       (if mutable?
	   (with-syntax
	       ((dummy-def (stepper-syntax-property
			    (syntax (define dummy (lambda () (set! var 'dummy))))
			    'stepper-skip-completely
			    #t)))
	     (syntax/loc stx
			 (begin
			   dummy-def
			   (define var expr))))
	   (syntax/loc stx (define var expr)))))
    ((DMdA-define v e1 e2 e3 ...)
     (raise-syntax-error
      #f "Definition mit mehr als zwei Operanden" stx))))

(define-syntax (DMdA-define stx)
  (transform-DMdA-define stx #f))

(define-syntax (DMdA-advanced-define stx)
  (transform-DMdA-define stx #t))

(define-syntax (DMdA-let stx)
  (syntax-case stx ()
    ((DMdA-let () body)
     (syntax/loc stx body))
    ((DMdA-let ((var expr) ...) body)
     (begin
       (check-for-id-list!
	(syntax->list (syntax (var ...)))
	"Kein Bezeichner in Let-Bindung")
       (syntax/loc stx ((lambda (var ...) body) expr ...))))
    ((DMdA-let ((var expr) ...) body1 body2 ...)
     (raise-syntax-error
      #f "Let-Ausdruck hat mehr als einen Ausdruck als Rumpf" stx))
    ((DMdA-let expr ...)
     (raise-syntax-error
      #f "Let-Ausdruck erwartet eine Liste von Bindungen (Paare aus Name und Ausdruck) und einen Rumpf" stx))))

(define-syntax (DMdA-let* stx)
  (syntax-case stx ()
    ((DMdA-let* () body)
     (syntax/loc stx body))
    ((DMdA-let* ((var1 expr1) (var2 expr2) ...) body)
     (begin
       (check-for-id!
	(syntax var1)
	"Kein Bezeichner in Let*-Bindung")
       (syntax/loc stx ((lambda (var1)
			  (DMdA-let* ((var2 expr2) ...) body))
			expr1))))
    ((DMdA-let* ((var expr) ...) body1 body2 ...)
     (raise-syntax-error
      #f "Let*-Ausdruck hat mehr als einen Ausdruck als Rumpf" stx))
    ((DMdA-let* expr ...)
     (raise-syntax-error
      #f "Let*-Ausdruck erwartet eine Liste von Bindungen (Paare aus Name und Ausdruck) und einen Rumpf" stx))))

(define-syntax (DMdA-letrec stx)
  (syntax-case stx ()
    ((DMdA-letrec ((var expr) ...) body)
     (begin
       (check-for-id-list!
	(syntax->list (syntax (var ...)))
	"Kein Bezeichner in letrec-Bindung")
       (syntax/loc stx (letrec ((var expr) ...) body))))
    ((DMdA-letrec ((var expr) ...) body1 body2 ...)
     (raise-syntax-error
      #f "Letrec hat mehr als einen Ausdruck als Rumpf" stx))))

(define-syntax (DMdA-lambda stx)
  (syntax-case stx ()
    ((DMdA-lambda (var ...) body)
     (begin
       (check-for-id-list!
	(syntax->list (syntax (var ...)))
	"Kein Bezeichner als Parameter der Lambda-Abstraktion")
       (syntax/loc stx (lambda (var ...) body))))
    ((DMdA-lambda (var ...) body1 body2 ...)
     (raise-syntax-error
      #f "Lambda-Abstraktion hat mehr als einen Ausdruck als Rumpf" stx))
    ((DMdA-lambda var body ...)
     (identifier? (syntax var))
     (raise-syntax-error
      #f "Um die Parameter einer Lambda-Abstraktion gehören Klammern" (syntax var)))
    ((DMdA-lambda var ...)
     (raise-syntax-error
      #f "Fehlerhafte Lambda-Abstraktion" stx))))

(define-syntax (DMdA-advanced-lambda stx)
  (syntax-case stx ()
    ((DMdA-lambda (var ...) body)
     (begin
       (check-for-id-list!
	(syntax->list (syntax (var ...)))
	"Kein Bezeichner als Parameter der Lambda-Abstraktion")
       (syntax/loc stx (lambda (var ...) body))))
    ((DMdA-lambda (var ... . rest) body)
     (begin
       (check-for-id-list!
	(syntax->list (syntax (var ...)))
	"Kein Bezeichner als Parameter der Lambda-Abstraktion")
       (check-for-id! 
	(syntax rest)
	"Kein Bezeichner als Restlisten-Parameter der Lambda-Abstraktion")
       (syntax/loc stx (lambda (var ... . rest) body))))
    ((DMdA-lambda (var ...) body1 body2 ...)
     (raise-syntax-error
      #f "Lambda-Abstraktion hat mehr als einen Ausdruck als Rumpf" stx))
    ((DMdA-lambda var ...)
     (raise-syntax-error
      #f "Fehlerhafte Lambda-Abstraktion" stx))))

(define-syntax (DMdA-begin stx)
  (syntax-case stx ()
    ((DMdA-begin)
     (raise-syntax-error
      #f "Begin-Ausdruck braucht mindestens einen Operanden" stx))
    ((DMdA-begin expr1 expr2 ...)
     (syntax/loc stx (begin expr1 expr2 ...)))))

(define-for-syntax (local-expand-for-error stx ctx stops)
  ;; This function should only be called in an 'expression
  ;;  context. In case we mess up, avoid bogus error messages.
  (when (memq (syntax-local-context) '(expression))
    (local-expand stx ctx stops)))

(define-for-syntax (ensure-expression stx k)
  (if (memq (syntax-local-context) '(expression))
      (k)
      (stepper-syntax-property #`(begin0 #,stx) 'stepper-skipto skipto/second)))

;; A consistent pattern for stepper-skipto:
(define-for-syntax (stepper-ignore-checker stx)
  (stepper-syntax-property stx 'stepper-skipto '(syntax-e cdr syntax-e cdr car)))

;; Raise a syntax error:
(define-for-syntax (teach-syntax-error form stx detail msg . args)
  (let ([form (if (eq? form '|function call|) ; ####
		  form
		  #f)] ; extract name from stx
	[msg (apply format msg args)])
    (if detail
	(raise-syntax-error form msg stx detail)
	(raise-syntax-error form msg stx))))

;; The syntax error when a form's name doesn't follow a "("
(define-for-syntax (bad-use-error name stx)
  (teach-syntax-error
   name
   stx
   #f
   "`~a' wurde an einer Stelle gefunden, die keiner offenen Klammer folgt"
   name))

;; Use for messages "expected ..., found <something else>"
(define-for-syntax (something-else v)
  (let ([v (syntax-e v)])
    (cond
     [(number? v) "eine Zahl"]
     [(string? v) "eine Zeichenkette"]
     [else "etwas anderes"])))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cond
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (DMdA-cond stx)
  (ensure-expression
   stx
   (lambda ()
     (syntax-case stx ()
       [(_)
	(teach-syntax-error
	 'cond
	 stx
	 #f
	 "Frage und eine Antwort nach `cond' erwartet, aber da ist nichts")]
       [(_ clause ...)
	(let* ([clauses (syntax->list (syntax (clause ...)))]
	       [check-preceding-exprs
		(lambda (stop-before)
		  (let/ec k
		    (for-each (lambda (clause)
				(if (eq? clause stop-before)
				    (k #t)
				    (syntax-case clause ()
				      [(question answer)
				       (begin
					 (unless (and (identifier? (syntax question))
						      (free-identifier=? (syntax question) #'DMdA-else))
					   (local-expand-for-error (syntax question) 'expression null))
					 (local-expand-for-error (syntax answer) 'expression null))])))
			      clauses)))])
	  (let ([checked-clauses
		 (map
		  (lambda (clause)
		    (syntax-case clause (DMdA-else)
		      [(DMdA-else answer)
		       (let ([lpos (memq clause clauses)])
			 (when (not (null? (cdr lpos)))
			   (teach-syntax-error
			    'cond
			    stx
			    clause
			    "`else'-Test gefunden, der nicht am Ende des `cond'-Ausdrucks steht"))
			 (with-syntax ([new-test (stepper-syntax-property (syntax #t) 'stepper-else #t)])
			   (syntax/loc clause (new-test answer))))]
		      [(question answer)
		       (with-syntax ([verified (stepper-ignore-checker (syntax (verify-boolean question 'cond)))])
			 (syntax/loc clause (verified answer)))]
		      [()
		       (check-preceding-exprs clause)
		       (teach-syntax-error
			'cond
			stx
			clause
			"Test und Ausdruck in Zweig erwartet, aber Zweig leer")]
		      [(question?)
		       (check-preceding-exprs clause)
		       (teach-syntax-error
			'cond
			stx
			clause
			"Zweig mit Test und Ausdruck erwartet, aber Zweig enthält nur eine Form")]
		      [(question? answer? ...)
		       (check-preceding-exprs clause)
		       (let ([parts (syntax->list clause)])
			 ;; to ensure the illusion of left-to-right checking, make sure 
			 ;; the question and first answer (if any) are ok:
			 (unless (and (identifier? (car parts))
				      (free-identifier=? (car parts) #'DMdA-else))
			   (local-expand-for-error (car parts) 'expression null))
			 (unless (null? (cdr parts))
			   (local-expand-for-error (cadr parts) 'expression null))
			 ;; question and answer (if any) are ok, raise a count-based exception:
			 (teach-syntax-error
			  'cond
			  stx
			  clause
			  "Zweig mit Test und Ausdruck erwartet, aber Zweig enthält ~a Formen"
			  (length parts)))]
		      [_else
		       (teach-syntax-error
			'cond
			stx
			clause
			"Zweig mit Test und Ausdruck erwartet, aber ~a gefunden"
			(something-else clause))]))
		  clauses)])
	    ;; Add `else' clause for error (always):
	    (let ([clauses (append checked-clauses 
				   (list 
				    (with-syntax ([error-call (syntax/loc stx (error 'cond "alle Tests ergaben #f"))])
				      (syntax [else error-call]))))])
	      (with-syntax ([clauses clauses])
		(syntax/loc stx (cond . clauses))))))]
       [_else (bad-use-error 'cond stx)]))))

(define-syntax DMdA-else
  (make-set!-transformer
   (lambda (stx)
     (define (bad expr)
       (teach-syntax-error
	'else
	expr
	#f
	"hier nicht erlaubt, weil kein Test in `cond'-Zweig"))
     (syntax-case stx (set! x)
       [(set! e expr) (bad #'e)]
       [(e . expr) (bad #'e)]
       [e (bad stx)]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (DMdA-if stx)
  (ensure-expression
   stx
   (lambda ()
     (syntax-case stx ()
       [(_ test then else)
	(with-syntax ([new-test (stepper-ignore-checker (syntax (verify-boolean test 'if)))])
	  (syntax/loc stx
		      (if new-test
			  then
			  else)))]
       [(_ . rest)
	(let ([n (length (syntax->list (syntax rest)))])
	  (teach-syntax-error
	   'if
	   stx
	   #f
	   "Test und zwei Ausdrücke erwartet, aber ~a Form~a gefunden"
	   (if (zero? n) "keine" n)
	   (if (= n 1) "" "en")))]
       [_else (bad-use-error 'if stx)]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; or, and
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntaxes (DMdA-or DMdA-and)
  (let ([mk
	 (lambda (where)
	   (let ([stepper-tag (case where
				[(or) 'comes-from-or]
				[(and) 'comes-from-and])])
	     (with-syntax ([swhere where])
	       (lambda (stx)
		 (ensure-expression
		  stx
		  (lambda ()
		    (syntax-case stx ()
		      [(_ . clauses)
		       (let ([n (length (syntax->list (syntax clauses)))])
			 (let loop ([clauses-consumed 0]
				    [remaining (syntax->list #`clauses)])
			   (if (null? remaining)
			       (case where
				 [(or) #`#f]
				 [(and) #`#t])
			       (stepper-syntax-property
				(stepper-syntax-property
				 (quasisyntax/loc 
				  stx
				  (if #,(stepper-ignore-checker (quasisyntax/loc stx (verify-boolean #,(car remaining) 'swhere)))
				      #,@(case where
					   [(or) #`(#t
						    #,(loop (+ clauses-consumed 1) (cdr remaining)))]
					   [(and) #`(#,(loop (+ clauses-consumed 1) (cdr remaining))
						     #f)])))
				 'stepper-hint
				 stepper-tag)
				'stepper-and/or-clauses-consumed
				clauses-consumed))))]
		      [_else (bad-use-error where stx)])))))))])
    (values (mk 'or) (mk 'and))))

;; verify-boolean is inserted to check for boolean results:
(define (verify-boolean b where)
  (if (or (eq? b #t) (eq? b #f))
      b
      (raise
       (make-exn:fail:contract
	(string->immutable-string
	 (format "~a: Testresultat ist nicht boolesch: ~e" where b))
	(current-continuation-marks)))))

(define-teach DMdA not
  (lambda (b)
    (verify-boolean b 'not)
    (not b)))

(define (boolean=? a b)
  (verify-boolean a 'boolean=?)
  (verify-boolean b 'boolean=?)
  (eq? a b))

(define-syntax (DMdA-app stx)
  (syntax-case stx ()
    ((_)
     (raise-syntax-error
      #f "Zusammengesetzte Form ohne Operator" (syntax/loc stx ())))
    ((_ datum1 datum2 ...)
     (let ((scm-datum (syntax->datum (syntax datum1))))
       (or (number? scm-datum)
	   (boolean? scm-datum)
	   (string? scm-datum)
	   (char? scm-datum)))
     (raise-syntax-error #f "Operator darf kein Literal sein" (syntax datum1)))
    ((_ datum1 datum2 ...)
     (syntax/loc stx (#%app datum1 datum2 ...)))))

(define-syntax (DMdA-top stx)
  (syntax-case stx ()
    ((_ . id)
     ;; If we're in a module, we'll need to check that the name
     ;;  is bound....
     (if (and (not (identifier-binding #'id))
	      (syntax-source-module #'id))
	 ;; ... but it might be defined later in the module, so
	 ;; delay the check.
	 (stepper-ignore-checker 
	  (syntax/loc stx (#%app values (DMdA-top-continue id))))
	 (syntax/loc stx (#%top . id))))))

(define-syntax (DMdA-top-continue stx)
  (syntax-case stx ()
    [(_ id)
     ;; If there's still no binding, it's an "unknown name" error.
     (if (not (identifier-binding #'id))
	 (raise-syntax-error #f "Ungebundene Variable" (syntax/loc stx id))
	 ;; Don't use #%top here; id might have become bound to something
	 ;;  that isn't a value.
	 #'id)]))

(define-teach DMdA write-string
  (lambda (s)
    (when (not (string? s))
      (error "Argument von write-string ist keine Zeichenkette"))
    (display s)))

(define (write-newline)
  (newline))

(define-record-procedures chocolate-cookie
  make-chocolate-cookie chocolate-cookie?
  (chocolate-cookie-chocolate chocolate-cookie-cookie))

(define-record-procedures computer
  make-computer computer?
  (computer-processor
   computer-ram
   computer-hard-drive))

(define (violation text)
  (error text))

(define (string->strings-list s)
  (map (lambda (c) (make-string 1 c)) (string->list s)))

(define (strings-list->string l)
  (if (null? l)
      ""
      (string-append (car l) (strings-list->string (cdr l)))))

(define integer (signature/arbitrary arbitrary-integer (predicate integer?)))
(define number (signature/arbitrary arbitrary-real (predicate number?)))
(define rational (signature/arbitrary arbitrary-rational (predicate rational?)))
(define real (signature/arbitrary arbitrary-real (predicate real?)))

(define (natural? x)
  (and (integer? x)
       (not (negative? x))))

(define natural (signature/arbitrary arbitrary-natural (predicate natural?)))

(define boolean (signature/arbitrary arbitrary-boolean (predicate boolean?)))

(define (true? x)
  (eq? x #t))

(define (false? x)
  (eq? x #f))

(define true (signature (one-of #t)))
(define false (signature (one-of #f)))

(define string (signature/arbitrary arbitrary-printable-ascii-string (predicate string?)))
(define symbol (signature/arbitrary arbitrary-symbol (predicate symbol?)))
(define empty-list (signature (one-of empty)))

(define unspecific (signature unspecific %unspecific))
(define any (signature any %any))

;; aus collects/lang/private/teach.rkt

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dots (.. and ... and .... and ..... and ......)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntax Identifier -> Expression
;; Produces an expression which raises an error reporting unfinished code.
(define-for-syntax (dots-error stx name)
  (quasisyntax/loc stx
		   (error (quote (unsyntax name))
			  "Fertiger Ausdruck erwartet, aber da sind noch Ellipsen")))

;; Expression -> Expression
;; Transforms unfinished code (... and the like) to code
;; raising an appropriate error.
(define-syntax DMdA-dots
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [(set! form expr) (dots-error stx (syntax form))]
       [(form . rest) (dots-error stx (syntax form))]
       [form (dots-error stx stx)]))))

(define-syntaxes (DMdA-set! DMdA-set!-continue)
  (let ((proc
	 (lambda (continuing?)
	   (lambda (stx)
	     (ensure-expression
	      stx
	      (lambda ()
		(syntax-case stx ()
		  ((_ id expr)
		   (identifier? (syntax id))
		   (begin
		     ;; Check that id isn't syntax, and not lexical.
		     ((with-handlers ((exn:fail? (lambda (exn) void)))
			;; First try syntax:
			;; If it's a transformer binding, then it can take care of itself...
			(if (set!-transformer? (syntax-local-value (syntax id)))
			    void  ;; no lex check wanted
			    (lambda ()
			      (raise-syntax-error
			       #f
			       "Nach set! wird eine gebundene Variable erwartet, aber da ist ein Schlüsselwort."
			       stx)))))
		     ;; If we're in a module, we'd like to check here whether
		     ;;  the identier is bound, but we need to delay that check
		     ;;  in case the id is defined later in the module. So only
		     ;;  do this in continuing mode:
		     (when continuing?
		       (when (and (not (identifier-binding #'id))
				  (syntax-source-module #'id))
			 (raise-syntax-error #f "Ungebundene Variable" #'id)))
		     (if continuing?
			 (syntax/loc stx (set! id expr))
			 (stepper-ignore-checker (syntax/loc stx (#%app values (DMdA-set!-continue id expr)))))))
		  ((_ id expr)
		   (raise-syntax-error
		    #f
		    "Nach set! wird eine Variable aber da ist etwas anderes."
		    #'id))
		  ((_ id)
		   (raise-syntax-error
		    #f
		    "Nach set! wird eine Variable und ein Ausdruck erwartet - der Ausdruck fehlt."
		    stx))
		  ((_)
		   (raise-syntax-error
		    #f
		    "Nach set! wird eine Variable und ein Ausdruck erwartet, aber da ist nichts."
		    stx))
		  (_else 
		   (raise-syntax-error
		    #f
		    "Inkorrekter set!-Ausdruck."
		    stx)))))))))
    (values (proc #f)
	    (proc #t))))

; QuickCheck

(define-syntax (for-all stx)
  (syntax-case stx ()
    ((_ (?clause ...) ?body)
     (with-syntax ((((?id ?arb) ...)
		    (map (lambda (pr)
			   (syntax-case pr ()
			     ((?id ?signature)
			      (identifier? #'?id)
			      (with-syntax ((?error-call
					     (syntax/loc #'?signature (error "Signatur hat keinen Generator"))))
				#'(?id
				   (or (signature-arbitrary (signature ?signature))
				       ?error-call))))
			     (_
			      (raise-syntax-error #f "inkorrekte `for-all'-Klausel - sollte die Form (id contr) haben"
						  pr))))
			 (syntax->list #'(?clause ...)))))

       (stepper-syntax-property #'(quickcheck:property 
				   ((?id ?arb) ...) ?body)
				'stepper-skip-completely
				#t)))
    ((_ ?something ?body)
     (raise-syntax-error #f "keine Klauseln der Form (id contr)"
			 stx))
    ((_ ?thing1 ?thing2 ?thing3 ?things ...)
     (raise-syntax-error #f "zuviele Operanden"
			 stx))))

(define-syntax (check-property stx)
  (unless (memq (syntax-local-context) '(module top-level))
    (raise-syntax-error
     #f "`check-property' muss ganz außen stehen" stx))
  (syntax-case stx ()
    ((_ ?prop)
     (stepper-syntax-property
      (check-expect-maker stx #'check-property-error #'?prop '() 
			  'comes-from-check-property)
      'stepper-replace
      #'#t))
    (_ (raise-syntax-error #f "`check-property' erwartet einen einzelnen Operanden"
			   stx))))

(define (check-property-error test src-info test-info)
  (let ((info (send test-info get-info)))
    (send info add-check)
    (with-handlers ((exn:fail?
		     (lambda (e)
		       (send info property-error e src-info)
		       (raise e))))
      (call-with-values
	  (lambda ()
	    (with-handlers
		((exn:assertion-violation?
		  (lambda (e)
		    ;; minor kludge to produce comprehensible error message
		    (if (eq? (exn:assertion-violation-who e) 'coerce->result-generator)
			(raise (make-exn:fail (string-append "Wert muß Eigenschaft oder boolesch sein: "
							     ((error-value->string-handler)
							      (car (exn:assertion-violation-irritants e))
							      100))
					      (exn-continuation-marks e)))
			(raise e)))))
	      (quickcheck-results (test))))
	(lambda (ntest stamps result)
	  (if (check-result? result)
	      (begin
		(send info property-failed result src-info)
		#f)
	      #t))))))

(define (expect v1 v2)
  (quickcheck:property () (teach-equal? v1 v2)))

(define (ensure-real who n val)
  (unless (real? val)
    (raise
     (make-exn:fail:contract
      (string->immutable-string
       (format "~a Argument ~e zu `~a' keine reelle Zahl." n val who))
      (current-continuation-marks)))))

(define (expect-within v1 v2 epsilon)
  (ensure-real 'expect-within "Drittes" epsilon)
  (quickcheck:property () (beginner-equal~? v1 v2 epsilon)))

(define (expect-range val min max)
  (ensure-real 'expect-range "Erstes" val)
  (ensure-real 'expect-range "Zweites" min)
  (ensure-real 'expect-range "Drittes" max)
  (quickcheck:property ()
		       (and (<= min val)
			    (<= val max))))

(define (expect-member-of val . candidates)
  (quickcheck:property () 
		       (ormap (lambda (cand)
				(teach-equal? val cand))
			      candidates)))

(define property (signature (predicate (lambda (x)
					(or (boolean? x)
					    (property? x))))))

