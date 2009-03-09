(module info-i mzscheme

   (provide (all-from-except mzscheme 
                             define let let* letrec lambda cond if begin 
                             display newline read #%app)
            symbol=?
            info-i-version
            (all-from "define-record-procedures.ss"))

  
   (define-syntax provide/rename
     (syntax-rules ()
       ((provide/rename (here there) ...)
        (begin
          (provide (rename here there)) ...))))

    (provide/rename
     (info-i-define define)
     (info-i-let let)
     (info-i-let* let*)
     (info-i-letrec letrec)
     (info-i-lambda lambda)
     (info-i-cond cond)
     (info-i-if if)
     (info-i-begin begin)
     (info-i-display display)
     (info-i-newline newline)
     (info-i-read read)
     (info-i-app #%app))

  (require "define-record-procedures.ss")

  (require-for-syntax "syntax-checkers.ss")

  (define-syntax (info-i-define stx)
    (syntax-case stx ()
      ((info-i-define)
       (raise-syntax-error
	#f "Define erwartet zwei Operanden, nicht null" stx))
      ((info-i-define v)
       (raise-syntax-error
	#f "Define erwartet zwei Operanden, nicht einen" stx))
      ((info-i-define (f arg ...) body)
       (begin
         (check-for-id! (syntax f)
                        "Funktionsname im define ist kein Bezeichner")
         (check-for-id-list!
          (syntax->list (syntax (arg ...)))
          "Argument im define ist kein Bezeichner")
         (syntax/loc stx (define (f arg ...) body))))
      ((info-i-define (f arg ... . rest) body)
       (begin
         (check-for-id! 
          (syntax f)
          "Funktionsname im define ist kein Bezeichner")
         (check-for-id-list!
          (syntax->list (syntax (arg ...)))
          "Argument im define ist kein Bezeichner")
         (check-for-id!
          (syntax rest)
          "Kein Bezeichern als Restlisten-Parameter von define")
         (syntax/loc stx (define (f arg ... . rest) body))))
      ((info-i-define (f arg ...) body1 body2 ...)
       (raise-syntax-error
        #f "Mehr als ein Ausdruck im Rumpf von define" stx))
      ((info-i-define var expr)
       (begin
         (check-for-id!
          (syntax var)
          "Der erste Operand von define ist kein Bezeichner")
         (syntax/loc stx (define var expr))))
      ((info-i-define v e1 e2 e3 ...)
       (raise-syntax-error
        #f "Define erwartet zwei Operanden, nicht" stx))))

  (define-syntax (info-i-let stx)
    (syntax-case stx ()
      ((info-i-let () body)
       (syntax/loc stx body))
      ((info-i-let ((var expr) ...) body)
       (begin
         (check-for-id-list!
          (syntax->list (syntax (var ...)))
          "Kein Bezeichner in let-Bindung")
         (syntax/loc stx ((lambda (var ...) body) expr ...))))
      ((info-i-let ((var expr) ...) body1 body2 ...)
       (raise-syntax-error
        #f "Let hat mehr als einen Ausdruck als Rumpf" stx))
      ((info-i-let expr ...)
       (raise-syntax-error
	#f "Let erwartet eine Liste von Bindungen (Paare aus Name und Ausdruck) und einen Rumpf" stx))))

  (define-syntax (info-i-let* stx)
    (syntax-case stx ()
      ((info-i-let* () body)
       (syntax/loc stx body))
      ((info-i-let* ((var1 expr1) (var2 expr2) ...) body)
       (begin
         (check-for-id!
          (syntax var1)
          "Kein Bezeichner in let*-Bindung")
         (syntax/loc stx ((lambda (var1)
			    (info-i-let* ((var2 expr2) ...) body))
			  expr1))))
      ((info-i-let* ((var expr) ...) body1 body2 ...)
       (raise-syntax-error
        #f "Let* hat mehr als einen Ausdruck als Rumpf" stx))
      ((info-i-let* expr ...)
       (raise-syntax-error
	#f "Let* erwartet eine Liste von Bindungen (Paare aus Name und Ausdruck) und einen Rumpf" stx))))

  (define-syntax (info-i-letrec stx)
    (syntax-case stx ()
      ((info-i-letrec ((var expr) ...) body)
       (begin
         (check-for-id-list!
          (syntax->list (syntax (var ...)))
          "Kein Bezeichner in letrec-Bindung")
         (syntax/loc stx (letrec ((var expr) ...) body))))
      ((info-i-letrec ((var expr) ...) body1 body2 ...)
       (raise-syntax-error
        #f "Letrec hat mehr als einen Ausdruck als Rumpf" stx))))

  (define-syntax (info-i-lambda stx)
    (syntax-case stx ()
      ((info-i-lambda (var ...) body)
       (begin
         (check-for-id-list!
          (syntax->list (syntax (var ...)))
          "Kein Bezeichner als Parameter von lambda")
         (syntax/loc stx (lambda (var ...) body))))
      ((info-i-lambda (var ... . rest) body)
       (begin
         (check-for-id-list!
          (syntax->list (syntax (var ...)))
          "Kein Bezeichner als Parameter von lambda")
         (check-for-id! 
          (syntax rest)
          "Kein Bezeichner als Restlisten-Parameter von lambda")
         (syntax/loc stx (lambda (var ... . rest) body))))
      ((info-i-lambda (var ...) body1 body2 ...)
        (raise-syntax-error
         #f "Lambda hat mehr als einen Ausdruck als Rumpf" stx))
      ((info-i-lambda var ...)
       (raise-syntax-error
	#f "Lambda erwartet eine Liste von Argumenten und einen Rumpf" stx))))

  (define-syntax (info-i-cond stx)
    (syntax-case stx (else)
      ((info-i-cond (else e)) 
       (syntax/loc stx e))
      ((info-i-cond)
       (syntax/loc stx (error "Kein Test im cond-Ausdruck war wahr")))
      ((info-i-cond (test rhs))
       (syntax/loc
	stx
	(if test 
	    rhs 
	    (info-i-cond))))
      ((info-i-cond (test rhs) clause1 clause2 ...)
       (syntax/loc
	stx
	(if test
	    rhs
	    (info-i-cond clause1 clause2 ...))))
      ((info-i-cond (test rhs1 rhs2 ...) clause1 ...)
       (raise-syntax-error
        #f "Mehr als eine Antwort im Cond" stx))))
      

  (define-syntax (info-i-if stx)
    (syntax-case stx ()
      ((info-i-if test cons)
       (raise-syntax-error
        #f "If braucht eine Alternative" stx))
      ((info-i-if test cons alt)
       (syntax/loc stx (if test cons alt)))
      ((info-i-if test cons alt1 alt2 ...)
       (raise-syntax-error
        #f "If mit mehr als drei Operanden" stx))
      ((info-i-if ...)
       (raise-syntax-error
	#f "If braucht drei Operanden" stx))))

  (define-syntax (info-i-begin stx)
    (syntax-case stx ()
      ((info-i-begin)
       (raise-syntax-error
        #f "Begin braucht mindestens einen Operanden" stx))
      ((info-i-begin expr1 expr2 ...)
       (syntax/loc stx (begin expr1 expr2 ...)))))

  (define-syntax (info-i-app stx)
    (syntax-case stx ()
      ((_)
       (raise-syntax-error
        #f "Zusammengesetzte Form ohne Operator" (syntax/loc stx ())))
      ((_ datum1 . datum2)
       (syntax/loc stx (#%app datum1 . datum2)))))

  (define (info-i-display e)
    (display e))
  
  (define (info-i-newline)
    (newline))

  (define (info-i-read)
    (read))

  (define (symbol=? s1 s2)
    (if (not (symbol? s1))
        (error "Erstes Argument von symbol=? ist kein Symbol"))
    (if (not (symbol? s2))
        (error "Zweites Argument von symbol=? ist kein Symbol"))
    (equal? s1 s2))

  (define info-i-version "27.1.2005")
)
