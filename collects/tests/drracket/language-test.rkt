#lang scheme

#|

Make sure there are tests that cover these parameters:

 (read-case-sensitive #f) 
 (read-square-bracket-as-paren #f)  -- test: (symbol? '[])
 (read-curly-brace-as-paren #f)
 (print-vector-length #f)

the settings above should match r5rs

|#


(require "private/drracket-test-util.rkt"
         tests/utils/gui
         mred
         framework
         (prefix-in fw: framework))

(define language (make-parameter "<<not a language>>"))
(define defs-prefix (make-parameter ""))

;; set-language : boolean -> void
(define (set-language close-dialog?)
  (if (eq? (car (language)) 'module)
      (set-module-language! close-dialog?)
      (set-language-level! (language) close-dialog?)))


;                                             
;                                             
;                                             
;                                             
;                        ;;         ;;        
;                        ;;         ;;        
;   ;;;;; ;;    ;;;   ;;;;;  ;; ;;  ;;   ;;;; 
;   ;;;;;;;;;  ;;;;;  ;;;;;  ;; ;;  ;;  ;; ;;;
;   ;; ;;  ;; ;;;  ;;;;; ;;  ;; ;;  ;; ;;;;;;;
;   ;; ;;  ;; ;;;  ;;;;; ;;  ;; ;;  ;; ;;;    
;   ;; ;;  ;;  ;;;;;  ;;;;;  ;;;;;  ;;  ;; ;; 
;   ;; ;;  ;;   ;;;   ;;;;;  ;;;;;  ;;   ;;;; 
;                                             
;                                             
;                                             
;                                             

(define (module-lang)
  (parameterize ([language '(module "racket")]
                 [defs-prefix "#lang racket\n"])
    
    (check-top-of-repl)
    
    (prepare-for-test-expression)
    
    (test-expression "'|.|" "'|.|")
    (test-expression '("(equal? (list " image ") (list " image "))") 
                     "#t")
    (test-expression "(define x 1)(define x 2)" #rx"duplicate definition for identifier in: x" "")
    
    (test-expression "(define-struct spider (legs))(make-spider 4)" 
                     "#<spider>"
                     #rx"define-values: cannot re-define a constant.*struct:spider")
    
    (test-expression "(sqrt -1)" "0+1i")
    
    (test-expression "class" (regexp "class: bad syntax in: class"))
    (test-expression "shared" (regexp "shared: bad syntax in: shared"))
    
    (test-expression "(define (. x y) (* x y))" #rx"read: illegal use of \"\\.\"" "")
    (test-expression "'(1 . 2)" "'(1 . 2)")
    
    (test-expression "(define (f define) 1)" "" #rx"define-values: cannot re-define a constant.*: f")
    (test-expression "(define (f car) 1)" "" #rx"define-values: cannot re-define a constant.*: f")
    (test-expression "(define (f empty) 1)" "" #rx"define-values: cannot re-define a constant.*: f")
    
    (test-expression "call/cc" "#<procedure:call-with-current-continuation>")
    
    (test-expression "(error 'a \"~a\" 1)" "{stop-multi.png} {stop-22x22.png} a: 1")
    (test-expression "(error \"a\" \"a\")" "{stop-multi.png} {stop-22x22.png} a \"a\"")
    
    (test-expression "(time 1)" 
                     #rx"cpu time: [0-9]+ real time: [0-9]+ gc time: [0-9]+\n1")
    
    (test-expression "true" "#t")
    (test-expression "mred^" 
                     #rx"unbound identifier in module in: mred\\^"
                     #rx"reference to undefined identifier.*: mred\\^")
    (test-expression "(eq? 'a 'A)" "#f")
    (test-expression "(set! x 1)" 
                     #rx"set!: unbound identifier in module in: x"
                     #rx"set!: cannot set undefined.*: x")
    (test-expression "(define qqq 2) (set! qqq 1)" "")
    (test-expression "(cond [(= 1 2) 3])" "")
    (test-expression "(cons 1 2)" "'(1 . 2)")
    (test-expression "(+ (list 1) 2)" #rx"[+]: contract violation.*given: '[(]1[)]")
    (test-expression "'(1)" "'(1)")
    (test-expression "(define shrd (box 1)) (list shrd shrd)"
                     "'(#&1 #&1)"
                     #rx"define-values: cannot re-define a constant.*: shrd")
    (test-expression "(local ((define x x)) 1)" "1")
    (test-expression "(letrec ([x x]) 1)" "1")
    (test-expression "(if 1 1 1)" "1")
    (test-expression "(+ 1)" "1")
    
    (test-expression "1.0" "1.0")
    (test-expression "#i1.0" "1.0")
    (test-expression "4/3" "{number 4/3 \"1 1/3\" mixed}")
    (test-expression "1/3" "{number 1/3 \"1/3\" mixed}")
    (test-expression "-4/3" "{number -4/3 \"-1 1/3\" mixed}")
    (test-expression "-1/3" "{number -1/3 \"-1/3\" mixed}")
    (test-expression "3/2" "{number 3/2 \"1 1/2\" mixed}")
    (test-expression "1/2" "{number 1/2 \"1/2\" mixed}")
    (test-expression "-1/2" "{number -1/2 \"-1/2\" mixed}")
    (test-expression "-3/2" "{number -3/2 \"-1 1/2\" mixed}")
    (test-expression "+1/3i" "0+1/3i")
    (test-expression "+1/2i" "0+1/2i")
    (test-expression "779625/32258" "{number 779625/32258 \"24 5433/32258\" mixed}")
    (test-expression "(exact? 1.5)" "#f")
    (test-expression "(print (floor (sqrt 2)))" "1.0")
    
    (test-expression "(let ([f (lambda (x) x)]) f)" "#<procedure:f>")
    (test-expression ",1" "{stop-22x22.png} unquote: not in quasiquote in: (unquote 1)")
    
    (test-expression "(list 1)" "'(1)")
    (test-expression "(car (list))" #rx"{stop-multi.png} {stop-22x22.png} car: contract violation.*given: '[(][)]")
    
    (test-expression "(current-command-line-arguments)" "'#()")
    (test-expression "(define-syntax app syntax-case)" "{stop-22x22.png} syntax-case: bad syntax in: syntax-case")
    
    (test-expression "#lang racket" #rx"read: #lang not enabled in the current context" "")
    (test-expression "(define (f)\n(+ (raise-user-error 'a \"b\")))\n(if (zero? (random 1)) (void) (set! f void))\n(f)"
                     "a: b")))


;                                                                               
;                                                                               
;                                                                               
;                                ;      ;               ;;;;        ;;          
;                               ;;     ;;               ;;;;        ;;          
;  ;;;;;;;   ;;; ;;;   ;;;    ;;;;;  ;;;;; ;;;  ;;;     ;;;;;;;         ;;;;;;; 
;  ;;;;;;;;  ;;;;;;;  ;;;;;  ;;;;;; ;;;;;; ;;; ;;;;     ;;;;;;;;  ;;;; ;;;;;;;; 
;  ;;;;;;;;; ;;;; ;; ;;;; ;;  ;;;;   ;;;;   ;;;;;;      ;;;;;;;;; ;;;; ;;; ;;;; 
;  ;;;; ;;;; ;;;;    ;;;;;;;  ;;;;   ;;;;   ;;;;;;      ;;;; ;;;; ;;;; ;;;;;;;; 
;  ;;;;;;;;; ;;;;    ;;;;;    ;;;;;  ;;;;;   ;;;;;      ;;;;;;;;; ;;;;  ;;;;;;; 
;  ;;;;;;;;  ;;;;     ;;;;;;  ;;;;;  ;;;;;   ;;;;       ;;;;;;;;  ;;;; ;   ;;;; 
;  ;;;;;;;   ;;;;      ;;;;    ;;;;   ;;;;   ;;;;       ;;;;;;;   ;;;; ;;;;;;;; 
;  ;;;;                                     ;;;;                       ;;;;;;;; 
;  ;;;;                                     ;;;;                        ;;;;;;  
;                                                                               

(define (pretty-big)
  (parameterize ([language (list #rx"Pretty Big")])
    
    (check-top-of-repl)
    
    (generic-settings #f)
    (generic-output #t #t #t #t)
    
    (test-hash-bang)
    (test-error-after-definition)
    
    (prepare-for-test-expression)
    
    (test-expression "'|.|" "|.|")
    (test-expression '("(equal? (list " image ") (list " image "))") 
                     "#t")
    (test-expression "(define x 1)(define x 2)" "")
    
    (test-expression "(define-struct spider (legs))(make-spider 4)" "#<spider>")
    
    (test-expression "(sqrt -1)" "0+1i")
    
    (test-expression "class" (regexp "class: bad syntax in: class"))
    (test-expression "shared" (regexp "shared: bad syntax in: shared"))
    
    (test-expression "(define (. x y) (* x y))" #rx"read: illegal use of \"\\.\"")
    (test-expression "'(1 . 2)" "(1 . 2)")
    
    (test-expression "(define (f define) 1)" "")
    (test-expression "(define (f car) 1)" "")
    (test-expression "(define (f empty) 1)" "")
    
    (test-expression "call/cc" "#<procedure:call-with-current-continuation>")
    
    (test-expression "(error 'a \"~a\" 1)" "{stop-multi.png} {stop-22x22.png} a: 1")
    (test-expression "(error \"a\" \"a\")" "{stop-multi.png} {stop-22x22.png} a \"a\"")
    
    (test-expression "(time 1)" 
                     #rx"cpu time: [0-9]+ real time: [0-9]+ gc time: [0-9]+\n1")
    
    (test-expression "true" "#t")
    (test-expression "mred^" #rx"{stop-multi.png} {stop-22x22.png} reference to undefined identifier.*: mred\\^")
    (test-expression "(eq? 'a 'A)" "#f")
    (test-expression "(set! x 1)" #rx"{stop-multi.png} {stop-22x22.png} set!: cannot set undefined.*: x")
    (test-expression "(define qqq 2) (set! qqq 1)" "")
    (test-expression "(cond [(= 1 2) 3])" "")
    (test-expression "(cons 1 2)" "(1 . 2)")
    (test-expression "(+ (list 1) 2)" #rx"{stop-multi.png} {stop-22x22.png} [+]: contract violation.*given: [(]1[)]")
    (test-expression "'(1)" "(1)")
    (test-expression "(define shrd (box 1)) (list shrd shrd)"
                     "(#&1 #&1)")
    (test-expression "(local ((define x x)) 1)" "1")
    (test-expression "(letrec ([x x]) 1)" "1")
    (test-expression "(if 1 1 1)" "1")
    (test-expression "(+ 1)" "1")
    
    (test-expression "1.0" "1.0")
    (test-expression "#i1.0" "1.0")
    (test-expression "4/3" "{number 4/3 \"1 1/3\" mixed}")
    (test-expression "1/3" "{number 1/3 \"1/3\" mixed}")
    (test-expression "-4/3" "{number -4/3 \"-1 1/3\" mixed}")
    (test-expression "-1/3" "{number -1/3 \"-1/3\" mixed}")
    (test-expression "3/2" "{number 3/2 \"1 1/2\" mixed}")
    (test-expression "1/2" "{number 1/2 \"1/2\" mixed}")
    (test-expression "-1/2" "{number -1/2 \"-1/2\" mixed}")
    (test-expression "-3/2" "{number -3/2 \"-1 1/2\" mixed}")
    (test-expression "+1/3i" "0+1/3i")
    (test-expression "+1/2i" "0+1/2i")
    (test-expression "779625/32258" "{number 779625/32258 \"24 5433/32258\" mixed}")
    (test-expression "(exact? 1.5)" "#f")
    (test-expression "(print (floor (sqrt 2)))" "1.0")
    
    (test-expression "(let ([f (lambda (x) x)]) f)" "#<procedure:f>")
    (test-expression ",1" "{stop-22x22.png} unquote: not in quasiquote in: (unquote 1)")
    
    (test-expression "(list 1)" "(1)")
    (test-expression "(car (list))" #rx"{stop-multi.png} {stop-22x22.png} car: contract violation.*given: [(][)]")
    
    (test-expression "(current-command-line-arguments)" "#()")
    (test-expression "(define-syntax app syntax-case)" "{stop-22x22.png} syntax-case: bad syntax in: syntax-case")
    
    (test-expression "#lang racket"
                     ""
                     #rx"read: #lang not enabled in the current context")
    (test-expression "(define (f)\n(+ (raise-user-error 'a \"b\")))\n(if (zero? (random 1)) (void) (set! f void))\n(f)"
                     "a: b")))

;                                      
;                                      
;  ;;;;;;;           ;;;;;;;    ;;;;;; 
;   ;;   ;;           ;;   ;;  ;;   ;; 
;   ;;   ;;    ;;;;;  ;;   ;;  ;;    ; 
;   ;;   ;;    ;;;;;  ;;   ;;  ;;;;    
;   ;;;;;;     ;      ;;;;;;    ;;;;;  
;   ;; ;;      ;;;;   ;; ;;        ;;; 
;   ;;  ;;        ;;  ;;  ;;   ;    ;; 
;   ;;   ;;       ;;  ;;   ;;  ;;   ;; 
;  ;;;;   ;;; ;;  ;; ;;;;   ;;;;;;;;;  
;             ;;  ;;                   
;              ;;;;                    
;                                      


(define (r5rs)
  (parameterize ([language (list (regexp "R5RS"))])
    
    (check-top-of-repl)
    
    (generic-settings #f)
    (generic-output #t #t #t #t)
    
    (test-hash-bang)
    (test-error-after-definition)
    
    (prepare-for-test-expression)
    
    (test-expression "'|.|" "|.|")
    (test-expression '("(equal? (list " image ") (list " image "))") 
                     "#t")
    (test-expression "(define x 1)(define x 2)" "")
    
    (test-expression 
     "(define-struct spider (legs))(make-spider 4)" 
     #rx"{stop-multi.png} {stop-22x22.png} reference to undefined identifier.*: define-struct")
    
    (test-expression "(sqrt -1)" "0+1i")
    
    (test-expression "class" #rx"{stop-multi.png} {stop-22x22.png} reference to undefined identifier.*: class")
    (test-expression "shared" #rx"{stop-multi.png} {stop-22x22.png} reference to undefined identifier.*: shared")
    
    (test-expression "(define (. x y) (* x y))" #rx"read: illegal use of \"\\.\"")
    (test-expression "'(1 . 2)" "(1 . 2)")
    
    (test-expression "(define (f define) 1)" "")
    (test-expression "(define (f car) 1)" "")
    (test-expression "(define (f empty) 1)" "")
    
    (test-expression "call/cc" #rx"{stop-multi.png} {stop-22x22.png} reference to undefined identifier.*: call/cc")
    
    (test-expression "(error 'a \"~a\" 1)" #rx"{stop-multi.png} {stop-22x22.png} reference to undefined identifier.*: error")
    (test-expression "(error \"a\" \"a\")" #rx"{stop-multi.png} {stop-22x22.png} reference to undefined identifier.*: error")
    
    (test-expression "(time 1)" 
                     #rx"{stop-multi.png} {stop-22x22.png} reference to undefined identifier.*: time")
    
    (test-expression "true" #rx"{stop-multi.png} {stop-22x22.png} reference to undefined identifier.*: true")
    (test-expression "mred^" #rx"{stop-multi.png} {stop-22x22.png} reference to undefined identifier.*: mred\\^")
    (test-expression "(eq? 'a 'A)" "#t")
    (test-expression "(set! x 1)" #rx"{stop-multi.png} {stop-22x22.png} set!: cannot set undefined.*: x")
    (test-expression "(define qqq 2) (set! qqq 1)" "")
    (test-expression "(cond ((= 1 2) 3))" "")
    (test-expression "(cons 1 2)" "(1 . 2)")
    (test-expression "(+ (list 1) 2)" #rx"{stop-multi.png} {stop-22x22.png} [+]: contract violation.*given: [(]1[)]")
    (test-expression "'(1)" "(1)")
    (test-expression "(define shrd (cons 1 1)) (list shrd shrd)"
                     "((1 . 1) (1 . 1))")
    (test-expression 
     "(local ((define x x)) 1)"
     #rx"define: not allowed in an expression context")
    (test-expression "(letrec ((x x)) 1)" "1")
    (test-expression "(if 1 1 1)" "1")
    (test-expression "(+ 1)" "1")
    
    (test-expression "1.0" "1.0")
    (test-expression "#i1.0" "1.0")
    (test-expression "4/3" "{number 4/3 \"1 1/3\" mixed}")
    (test-expression "1/3" "{number 1/3 \"1/3\" mixed}")
    (test-expression "-4/3" "{number -4/3 \"-1 1/3\" mixed}")
    (test-expression "-1/3" "{number -1/3 \"-1/3\" mixed}")
    (test-expression "3/2" "{number 3/2 \"1 1/2\" mixed}")
    (test-expression "1/2" "{number 1/2 \"1/2\" mixed}")
    (test-expression "-1/2" "{number -1/2 \"-1/2\" mixed}")
    (test-expression "-3/2" "{number -3/2 \"-1 1/2\" mixed}")
    (test-expression "+1/3i" "0+1/3i")
    (test-expression "+1/2i" "0+1/2i")
    (test-expression "779625/32258" "{number 779625/32258 \"24 5433/32258\" mixed}")
    (test-expression "(exact? 1.5)" "#f")
    (test-expression "(print (floor (sqrt 2)))" #rx"reference to undefined identifier.*: print")
    
    (test-expression "(let ((f (lambda (x) x))) f)" "#<procedure:f>")
    (test-expression ",1" "{stop-22x22.png} unquote: not in quasiquote in: (unquote 1)")
    
    (test-expression "(list 1)" "(1)")
    (test-expression "(car (list))"
                     #rx"{stop-multi.png} {stop-22x22.png} mcar: contract violation.*given: [(][)]")
    
    (test-expression "argv" #rx"{stop-multi.png} {stop-22x22.png} reference to undefined identifier.*: argv")
    (test-expression "(define-syntax app syntax-case)" 
                     "{stop-22x22.png} macro-transformer: only a `syntax-rules' form is allowed in: syntax-case")
    
    (test-expression "#lang racket"
                     (regexp (regexp-quote "#%module-begin: illegal use (not a module body) in: (#%module-begin)"))
                     #rx"read: #lang not enabled in the current context")
    (test-expression "(define (f)\n(+ (raise-user-error 'a \"b\")))\n(if (zero? (random 1)) (void) (set! f void))\n(f)"
                     #rx"reference to undefined identifier")))


;                                                             
;  ;;;                    ;;                                  
;   ;;                    ;;                                  
;   ;;                                                        
;   ;;;;;   ;;;;   ;;;;;;;;;  ;;; ;;   ;;; ;;    ;;;;  ;;; ;; 
;   ;;  ;; ;;  ;; ;;  ;;  ;;   ;;; ;;   ;;; ;;  ;;  ;;  ;;;;; 
;   ;;  ;; ;;  ;; ;;  ;;  ;;   ;;  ;;   ;;  ;;  ;;  ;;  ;;    
;   ;;  ;; ;;;;;;  ;;;;   ;;   ;;  ;;   ;;  ;;  ;;;;;;  ;;    
;   ;;  ;; ;;      ;      ;;   ;;  ;;   ;;  ;;  ;;      ;;    
;   ;;  ;; ;;   ;  ;;;;;  ;;   ;;  ;;   ;;  ;;  ;;   ;  ;;    
;   ;;;;;   ;;;;   ;;;;;;;;;; ;;;; ;;; ;;;; ;;;  ;;;;  ;;;;   
;                 ;;   ;;                                     
;                 ;;   ;;                                     
;                  ;;;;;                                      

(define (beginner)
  (parameterize ([language (list "How to Design Programs" #rx"Beginning Student(;|$)")])
    (check-top-of-repl)
    (generic-settings #t)
    (generic-output #f #f #f #f)
    (teaching-language-fraction-output)
    
    (test-hash-bang)
    (test-error-after-definition)
    
    (prepare-for-test-expression)
    
    (test-expression "'|.|" 
                     "'|.|"
                     "'|.|")
    (test-expression '("(equal? (list " image ") (list " image "))") 
                     "true"
                     "true")
    
    (test-expression "(define x 1)(define x 2)"
                     "x: this name was defined previously and cannot be re-defined"
                     "x: this name was defined previously and cannot be re-defined")
    
    (test-expression "(define-struct spider (legs))(make-spider 4)" 
                     "(make-spider 4)"
                     "spider: this name was defined previously and cannot be re-defined")
    
    (test-expression "(sqrt -1)" 
                     "0+1i"
                     "0+1i\n")
    
    (test-undefined-var "class") 
    (test-undefined-var "shared")
    (test-expression "(define (. x y) (* x y))" "read: illegal use of \".\"")
    (test-expression "'(1 . 2)"  "read: illegal use of \".\"")
    
    (test-undefined-var "call/cc")
    
    (test-expression "(error 'a \"~a\" 1)" "a: ~a1")
    (test-expression "(error \"a\" \"a\")" "aa")
    
    (test-undefined-fn "(time 1)" "time"))
    
    (test-expression "true" 
                     "true"
                     "true")
    (test-undefined-var "mred^")
    (test-expression "(eq? 'a 'A)" 
                     "false"
                     "false")
    (test-undefined-fn "(set! x 1)" "set!")
    (test-undefined-fn "(define qqq 2) (set! qqq 1)" "set!")
    
    (test-expression "(cond [(= 1 2) 3])"
                     "cond: all question results were false")
    (test-expression "(cons 1 2)" 
                     "cons: second argument must be a list, but received 1 and 2")
    (test-expression "(+ (list 1) 2)"
                     "+: expects a number as 1st argument, given (cons 1 empty)")
    (test-expression "'(1)"
                     "quote: expected the name of the symbol after the quote, but found a part")
    (test-expression "(define shrd (list 1)) (list shrd shrd)"
                     "(cons (cons 1 empty) (cons (cons 1 empty) empty))"
                     "shrd: this name was defined previously and cannot be re-defined")
    (test-expression "(local ((define x x)) 1)" 
                     "local: this function is not defined" 
                     "function call: expected a function after the open parenthesis, but found a part")
    (test-expression "(letrec ([x x]) 1)" 
                     "letrec: this function is not defined"
                     "function call: expected a function after the open parenthesis, but found a part")
    (test-expression "(if 1 1 1)" "if: question result is not true or false: 1")
    (test-expression "(+ 1)" "+: expects at least 2 arguments, but found only 1")
    
    (test-expression "1.0" "1" "1")
    (test-expression "#i1.0" "#i1.0" "#i1.0")
    (test-expression "4/3" 
                     "{number 4/3 \"1.3\" decimal}"
                     "{number 4/3 \"1.3\" decimal}")
    (test-expression "1/3"
                     "{number 1/3 \"0.3\" decimal}"
                     "{number 1/3 \"0.3\" decimal}")
    (test-expression "-4/3" 
                     "{number -4/3 \"-1.3\" decimal}"
                     "{number -4/3 \"-1.3\" decimal}")
    (test-expression "-1/3" 
                     "{number -1/3 \"-0.3\" decimal}"
                     "{number -1/3 \"-0.3\" decimal}")
    (test-expression "3/2" 
                     "{number 3/2 \"1.5\" decimal}"
                     "{number 3/2 \"1.5\" decimal}")
    (test-expression "1/2" 
                     "{number 1/2 \"0.5\" decimal}"
                     "{number 1/2 \"0.5\" decimal}")
    (test-expression "-1/2" 
                     "{number -1/2 \"-0.5\" decimal}"
                     "{number -1/2 \"-0.5\" decimal}")
    (test-expression "-3/2" 
                     "{number -3/2 \"-1.5\" decimal}"
                     "{number -3/2 \"-1.5\" decimal}")
    (test-expression "+1/3i" 
                     "0+1/3i"
                     "0+1/3i")
    (test-expression "+1/2i" 
                     "0+0.5i"
                     "0+0.5i")
    (test-expression "779625/32258"
                     "{number 779625/32258 \"24.1684233368466736933473866...\" decimal}"
                     "{number 779625/32258 \"24.1684233368466736933473866...\" decimal}")
    (test-expression "(exact? 1.5)" "true")
    (test-undefined-fn "(print (floor (sqrt 2)))" "print")
    
    (test-expression "(let ([f (lambda (x) x)]) f)"
                     "let: this function is not defined"
                     "function call: expected a function after the open parenthesis, but found a part")
    (test-expression ",1"
                     "read: illegal use of comma")
    
    (test-expression "(list 1)" 
                     "(cons 1 empty)"
                     "(cons 1 empty)")
    (test-expression "(car (list))" 
                     "car: expects a pair, given empty")
    
    (test-undefined-var "argv")
    (test-undefined-fn "(define-syntax app syntax-case)" "define-syntax")
    
    (test-expression "#lang racket"
                     "read: #lang not enabled in the current context"
                     "read: #lang not enabled in the current context")
    (test-expression "(define (f)\n(+ (raise-user-error 'a \"b\")))\n(if (zero? (random 1)) (void) (set! f void))\n(f)"
                       "define: expected at least one variable after the function name, but found none"
                       #rx"define: function definitions are not allowed in the interactions window"))


;                                                                            
;  ;;;                                 ;;;     ;;;                           
;   ;;                       ;          ;;      ;;                           
;   ;;                       ;          ;;      ;;                           
;   ;;;;;   ;;;;   ;;;;;;   ;   ;;;;    ;;;;;   ;;;;;  ;;; ;;  ;;;;  ;;; ;;; 
;   ;;  ;; ;;  ;; ;;  ;;    ;      ;;   ;;  ;;  ;;  ;;  ;;;;; ;;  ;;  ;;  ;  
;   ;;  ;; ;;  ;; ;;  ;;   ;       ;;   ;;  ;;  ;;  ;;  ;;    ;;  ;;  ;;  ;  
;   ;;  ;; ;;;;;;  ;;;;    ;    ;;;;;   ;;  ;;  ;;  ;;  ;;    ;;;;;;   ;;;   
;   ;;  ;; ;;      ;       ;   ;;  ;;   ;;  ;;  ;;  ;;  ;;    ;;       ;;;   
;   ;;  ;; ;;   ;  ;;;;;  ;    ;;  ;;   ;;  ;;  ;;  ;;  ;;    ;;   ;   ;;;   
;   ;;;;;   ;;;;   ;;;;;; ;     ;;;;;;  ;;;;;   ;;;;;  ;;;;    ;;;;     ;    
;                 ;;   ;;;                                                   
;                 ;;   ;;;                                                   
;                  ;;;;;                                                     


(define (beginner/abbrev)
  (parameterize ([language (list "How to Design Programs" 
                                 #rx"Beginning Student with List Abbreviations(;|$)")])
    (check-top-of-repl)
    
    (generic-settings #t)
    (generic-output #t #f #f #f)
    (teaching-language-fraction-output)
    
    (test-hash-bang)
    
    (test-error-after-definition)
    
    (prepare-for-test-expression)
    
    (test-expression "'|.|" 
                     "'|.|"
                     "'|.|")
    (test-expression '("(equal? (list " image ") (list " image "))") 
                     "true"
                     "true")
    
    (test-expression "(define x 1)(define x 2)"
                     "x: this name was defined previously and cannot be re-defined"
                     "x: this name was defined previously and cannot be re-defined")
    
    (test-expression "(define-struct spider (legs))(make-spider 4)"
                     "(make-spider 4)"
                     "spider: this name was defined previously and cannot be re-defined")
    
    (test-expression "(sqrt -1)" 
                     "0+1i"
                     "0+1i")
    
    (test-undefined-var "class")
    (test-undefined-var "shared")
    
    (test-expression "(define (. x y) (* x y))"  "read: illegal use of \".\"")
    (test-expression "'(1 . 2)"  "read: illegal use of \".\"")
    
    (test-undefined-var "call/cc")
    
    (test-expression "(error 'a \"~a\" 1)" "a: ~a1")
    (test-expression "(error \"a\" \"a\")" "aa")
    
    (test-undefined-fn "(time 1)" "time")
    
    (test-expression "true" 
                     "true"
                     "true")
    (test-undefined-var "mred^")
    (test-expression "(eq? 'a 'A)" 
                     "false"
                     "false")
    (test-undefined-fn "(set! x 1)" "set!")
    (test-undefined-fn "(define qqq 2) (set! qqq 1)" "set!")
    (test-expression "(cond [(= 1 2) 3])" "cond: all question results were false")
    (test-expression "(cons 1 2)" "cons: second argument must be a list, but received 1 and 2")
    (test-expression "(+ (list 1) 2)" "+: expects a number as 1st argument, given (list 1)")
    (test-expression "'(1)" 
                     "(list 1)"
                     "(list 1)")
    (test-expression "(define shrd (list 1)) (list shrd shrd)"
                     "(list (list 1) (list 1))"
                     "shrd: this name was defined previously and cannot be re-defined")
    (test-expression "(local ((define x x)) 1)"
                     "local: this function is not defined"
                     "function call: expected a function after the open parenthesis, but found a part")
    (test-expression "(letrec ([x x]) 1)"
                     "letrec: this function is not defined"
                     "function call: expected a function after the open parenthesis, but found a part")
    (test-expression "(if 1 1 1)" "if: question result is not true or false: 1")
    (test-expression "(+ 1)" "+: expects at least 2 arguments, but found only 1")
    
    (test-expression "1.0" "1" "1")
    (test-expression "#i1.0" "#i1.0" "#i1.0")
    (test-expression "4/3" 
                     "{number 4/3 \"1.3\" decimal}"
                     "{number 4/3 \"1.3\" decimal}")
    (test-expression "1/3" 
                     "{number 1/3 \"0.3\" decimal}"
                     "{number 1/3 \"0.3\" decimal}")
    (test-expression "-4/3" 
                     "{number -4/3 \"-1.3\" decimal}"
                     "{number -4/3 \"-1.3\" decimal}")
    (test-expression "-1/3" 
                     "{number -1/3 \"-0.3\" decimal}"
                     "{number -1/3 \"-0.3\" decimal}")
    (test-expression "3/2" 
                     "{number 3/2 \"1.5\" decimal}"
                     "{number 3/2 \"1.5\" decimal}")
    (test-expression "1/2" 
                     "{number 1/2 \"0.5\" decimal}"
                     "{number 1/2 \"0.5\" decimal}")
    (test-expression "-1/2" 
                     "{number -1/2 \"-0.5\" decimal}"
                     "{number -1/2 \"-0.5\" decimal}")
    (test-expression "-3/2" 
                     "{number -3/2 \"-1.5\" decimal}"
                     "{number -3/2 \"-1.5\" decimal}")
    (test-expression "+1/3i" 
                     "0+1/3i"
                     "0+1/3i")
    (test-expression "+1/2i" 
                     "0+0.5i"
                     "0+0.5i")
    (test-expression "779625/32258"
                     "{number 779625/32258 \"24.1684233368466736933473866...\" decimal}"
                     "{number 779625/32258 \"24.1684233368466736933473866...\" decimal}")
    (test-expression "(exact? 1.5)" "true")
    (test-undefined-fn "(print (floor (sqrt 2)))" "print")
    
    (test-expression "(let ([f (lambda (x) x)]) f)" 
                     "let: this function is not defined"
                       "function call: expected a function after the open parenthesis, but found a part")
    (test-expression ",1"
                     "unquote: misuse of a comma or unquote, not under a quasiquoting backquote")
    
    (test-expression "(list 1)" 
                     "(list 1)"
                     "(list 1)")
    (test-expression "(car (list))" "car: expects a pair, given empty")
    
    (test-undefined-var "argv")
    
    (test-undefined-fn "(define-syntax app syntax-case)" "define-syntax")
    
    (test-expression "#lang racket"
                     "read: #lang not enabled in the current context"
                     "read: #lang not enabled in the current context")
    (test-expression "(define (f)\n(+ (raise-user-error 'a \"b\")))\n(if (zero? (random 1)) (void) (set! f void))\n(f)"
                     "define: expected at least one variable after the function name, but found none"
                     #rx"define: function definitions are not allowed in the interactions window")))


;                                                                                          
;   ;;                                                      ;;;   ;;                       
;   ;;            ;;                                         ;;   ;;           ;;          
;                 ;;                                         ;;                ;;          
;  ;;;  ;;; ;;   ;;;;;  ;;;;  ;;; ;; ;;; ;;  ;;    ;;;;   ;;;;;  ;;;   ;;;;   ;;;;;  ;;;;  
;   ;;   ;;; ;;   ;;   ;;  ;;  ;;;;;  ;;; ;;; ;;  ;;  ;; ;;  ;;   ;;      ;;   ;;   ;;  ;; 
;   ;;   ;;  ;;   ;;   ;;  ;;  ;;     ;;  ;;  ;;  ;;  ;; ;;  ;;   ;;      ;;   ;;   ;;  ;; 
;   ;;   ;;  ;;   ;;   ;;;;;;  ;;     ;;  ;;  ;;  ;;;;;; ;;  ;;   ;;   ;;;;;   ;;   ;;;;;; 
;   ;;   ;;  ;;   ;;   ;;      ;;     ;;  ;;  ;;  ;;     ;;  ;;   ;;  ;;  ;;   ;;   ;;     
;   ;;   ;;  ;;   ;;   ;;   ;  ;;     ;;  ;;  ;;  ;;   ; ;;  ;;   ;;  ;;  ;;   ;;   ;;   ; 
;  ;;;; ;;;; ;;;   ;;;  ;;;;  ;;;;   ;;;; ;;; ;;;  ;;;;   ;;;;;; ;;;;  ;;;;;;   ;;;  ;;;;  
;                                                                                          
;                                                                                          
;                                                                                          


(define (intermediate)
  (parameterize ([language (list "How to Design Programs" #rx"Intermediate Student(;|$)")])
    (check-top-of-repl)
    
    (generic-settings #t)
    (generic-output #t #f #f #f)
    (teaching-language-fraction-output)
    
    (test-hash-bang)
    (test-error-after-definition)
    
    (prepare-for-test-expression)
    
    (test-expression "'|.|" 
                     "'|.|"
                     "'|.|")
    (test-expression '("(equal? (list " image ") (list " image "))") 
                     "true"
                     "true")
    
    (test-expression "(define x 1)(define x 2)"
                     "x: this name was defined previously and cannot be re-defined"
                     "x: this name was defined previously and cannot be re-defined")
    
    (test-expression "(define-struct spider (legs))(make-spider 4)"
                     "(make-spider 4)"
                     "spider: this name was defined previously and cannot be re-defined")
    
    (test-expression "(sqrt -1)" 
                     "0+1i"
                     "0+1i")
    
    (test-undefined-var "class")
    (test-undefined-var "shared")
    
    (test-expression "(define (. x y) (* x y))"  "read: illegal use of \".\"")
    (test-expression "'(1 . 2)"  "read: illegal use of \".\"")
    
    (test-undefined-var "call/cc")
    
    (test-expression "(error 'a \"~a\" 1)" "a: ~a1")
    (test-expression "(error \"a\" \"a\")" "aa")
    
    (test-expression "(time 1)" 
                     #rx"cpu time: [0-9]+ real time: [0-9]+ gc time: [0-9]+\n1")
    
    (test-expression "true" 
                     "true"
                     "true")
    (test-undefined-var "mred^")
    (test-expression "(eq? 'a 'A)" 
                     "false"
                     "false")
    (test-undefined-fn "(set! x 1)" "set!")
    (test-undefined-fn "(define qqq 2) (set! qqq 1)" "set!")
    (test-expression "(cond [(= 1 2) 3])" "cond: all question results were false")
    (test-expression "(cons 1 2)"   "cons: second argument must be a list, but received 1 and 2")
    (test-expression "(+ (list 1) 2)" "+: expects a number as 1st argument, given (list 1)")
    (test-expression "'(1)" 
                     "(list 1)"
                     "(list 1)")
    (test-expression "(define shrd (list 1)) (list shrd shrd)"
                     "(list (list 1) (list 1))"
                     "shrd: this name was defined previously and cannot be re-defined")
    (test-expression "(local ((define x x)) 1)" "local variable used before its definition: x")
    (test-expression "(letrec ([x x]) 1)" "local variable used before its definition: x")
    (test-expression "(if 1 1 1)" "if: question result is not true or false: 1")
    (test-expression "(+ 1)" "1")
    
    (test-expression "1.0" "1" "1")
    (test-expression "#i1.0" "#i1.0" "#i1.0")
    (test-expression "4/3" 
                     "{number 4/3 \"1.3\" decimal}"
                     "{number 4/3 \"1.3\" decimal}")
    (test-expression "1/3" 
                     "{number 1/3 \"0.3\" decimal}"
                     "{number 1/3 \"0.3\" decimal}")
    (test-expression "-4/3" 
                     "{number -4/3 \"-1.3\" decimal}"
                     "{number -4/3 \"-1.3\" decimal}")
    (test-expression "-1/3" 
                     "{number -1/3 \"-0.3\" decimal}"
                     "{number -1/3 \"-0.3\" decimal}")
    (test-expression "3/2" 
                     "{number 3/2 \"1.5\" decimal}"
                     "{number 3/2 \"1.5\" decimal}")
    (test-expression "1/2" 
                     "{number 1/2 \"0.5\" decimal}"
                     "{number 1/2 \"0.5\" decimal}")
    (test-expression "-1/2" 
                     "{number -1/2 \"-0.5\" decimal}"
                     "{number -1/2 \"-0.5\" decimal}")
    (test-expression "-3/2" 
                     "{number -3/2 \"-1.5\" decimal}"
                     "{number -3/2 \"-1.5\" decimal}")
    (test-expression "+1/3i" 
                     "0+1/3i"
                     "0+1/3i")
    (test-expression "+1/2i" 
                     "0+0.5i"
                     "0+0.5i")
    (test-expression "779625/32258" 
                     "{number 779625/32258 \"24.1684233368466736933473866...\" decimal}"
                     "{number 779625/32258 \"24.1684233368466736933473866...\" decimal}")
    (test-expression "(exact? 1.5)" "true")
    (test-undefined-fn "(print (floor (sqrt 2)))" "print")
    
    (test-expression "(let ([f (lambda (x) x)]) f)" 
                     "function:f"
                     "function:f")
    (test-expression ",1"
                     "unquote: misuse of a comma or unquote, not under a quasiquoting backquote")
    
    (test-expression "(list 1)" 
                     "(list 1)"
                     "(list 1)")
    (test-expression "(car (list))" "car: expects a pair, given empty")
    (test-undefined-var "argv")
    
    (test-undefined-fn "(define-syntax app syntax-case)" "define-syntax")
    
    (test-expression "#lang racket"
                     "read: #lang not enabled in the current context"
                     "read: #lang not enabled in the current context")
    (test-expression "(define (f)\n(+ (raise-user-error 'a \"b\")))\n(if (zero? (random 1)) (void) (set! f void))\n(f)"
                     "define: expected at least one variable after the function name, but found none"
                     #rx"define: expected at least one variable after the function name, but found none")))



;                                                                                    
;                                                                                    
;                                                                                    
;    ;;               ;   ;;;;;;                        ;;;;           ;;;;          
;    ;;              ;;   ;;;;;;                        ;;;;           ;;;;          
;       ;;;; ;;;   ;;;;;  ;;;;;; ;;;;;;;  ;;;;;;; ;;;;  ;;;;;;;     ;;;;;;; ;;;;;;;  
;  ;;;; ;;;;;;;;; ;;;;;;  ;;;;;; ;;;;;;;; ;;;;;;;;;;;;; ;;;;;;;;   ;;;;;;;; ;;;;;;;; 
;  ;;;; ;;;; ;;;;  ;;;;   ;;;;;;     ;;;; ;;;; ;;; ;;;; ;;;;;;;;; ;;;;;;;;;     ;;;; 
;  ;;;; ;;;; ;;;;  ;;;;  ;; ;;;;  ;;;;;;; ;;;; ;;; ;;;; ;;;; ;;;; ;;;; ;;;;  ;;;;;;; 
;  ;;;; ;;;; ;;;;  ;;;;; ;; ;;;; ;;  ;;;; ;;;; ;;; ;;;; ;;;;;;;;; ;;;;;;;;; ;;  ;;;; 
;  ;;;; ;;;; ;;;;  ;;;;; ;; ;;;; ;;;;;;;; ;;;; ;;; ;;;; ;;;;;;;;   ;;;;;;;; ;;;;;;;; 
;  ;;;; ;;;; ;;;;   ;;;; ;; ;;;;  ;; ;;;; ;;;; ;;; ;;;; ;;;;;;;     ;;;;;;;  ;; ;;;; 
;                        ;;                                                          
;                                                                                    
;                                                                                    


(define (intermediate/lambda)
  (parameterize ([language (list "How to Design Programs" 
                                 #rx"Intermediate Student with lambda(;|$)")])
    (check-top-of-repl)
    
    (generic-settings #t)
    (generic-output #t #f #f #f)
    (teaching-language-fraction-output)
    
    (test-hash-bang)
    (test-error-after-definition)
    
    (prepare-for-test-expression)
    
    (test-expression "'|.|"
                     "'|.|"
                     "'|.|")
    (test-expression '("(equal? (list " image ") (list " image "))") 
                     "true"
                     "true")
    (test-expression "(define x 1)(define x 2)"
                     "x: this name was defined previously and cannot be re-defined"
                     "x: this name was defined previously and cannot be re-defined")
    
    (test-expression "(define-struct spider (legs))(make-spider 4)"
                     "(make-spider 4)"
                     "spider: this name was defined previously and cannot be re-defined")
    
    (test-expression "(sqrt -1)" 
                     "0+1i"
                     "0+1i")
    
    (test-undefined-var "class")
    (test-undefined-var "shared")
    
    (test-expression "(define (. x y) (* x y))" "read: illegal use of \".\"")
    (test-expression "'(1 . 2)" "read: illegal use of \".\"")
    
    (test-undefined-var "call/cc")
    
    (test-expression "(error 'a \"~a\" 1)" "a: ~a1")
    (test-expression "(error \"a\" \"a\")" "aa")
    
    (test-expression "(time 1)" 
                     #rx"cpu time: [0-9]+ real time: [0-9]+ gc time: [0-9]+\n1")
    
    (test-expression "true" 
                     "true"
                     "true")
    (test-undefined-var "mred^")
    (test-expression "(eq? 'a 'A)" 
                     "false"
                     "false")
    (test-undefined-fn "(set! x 1)" "set!")
    (test-undefined-fn "(define qqq 2) (set! qqq 1)" "set!")
    (test-expression "(cond [(= 1 2) 3])" "cond: all question results were false")
    (test-expression "(cons 1 2)" "cons: second argument must be a list, but received 1 and 2")
    (test-expression "(+ (list 1) 2)" "+: expects a number as 1st argument, given (list 1)")
    (test-expression "'(1)" 
                     "(list 1)"
                     "(list 1)")
    (test-expression "(define shrd (list 1)) (list shrd shrd)"
                     "(list (list 1) (list 1))"
                     "shrd: this name was defined previously and cannot be re-defined")
    (test-expression "(local ((define x x)) 1)" "local variable used before its definition: x")
    (test-expression "(letrec ([x x]) 1)" "local variable used before its definition: x")
    (test-expression "(if 1 1 1)" "if: question result is not true or false: 1")
    (test-expression "(+ 1)" "1")
    
    (test-expression "1.0" "1" "1")
    (test-expression "#i1.0" "#i1.0" "#i1.0")
    (test-expression "4/3" 
                     "{number 4/3 \"1.3\" decimal}"
                     "{number 4/3 \"1.3\" decimal}")
    (test-expression "1/3" 
                     "{number 1/3 \"0.3\" decimal}"
                     "{number 1/3 \"0.3\" decimal}")
    (test-expression "-4/3" 
                     "{number -4/3 \"-1.3\" decimal}"
                     "{number -4/3 \"-1.3\" decimal}")
    (test-expression "-1/3" 
                     "{number -1/3 \"-0.3\" decimal}"
                     "{number -1/3 \"-0.3\" decimal}")
    (test-expression "3/2" 
                     "{number 3/2 \"1.5\" decimal}"
                     "{number 3/2 \"1.5\" decimal}")
    (test-expression "1/2" 
                     "{number 1/2 \"0.5\" decimal}"
                     "{number 1/2 \"0.5\" decimal}")
    (test-expression "-1/2" 
                     "{number -1/2 \"-0.5\" decimal}"
                     "{number -1/2 \"-0.5\" decimal}")
    (test-expression "-3/2" 
                     "{number -3/2 \"-1.5\" decimal}"
                     "{number -3/2 \"-1.5\" decimal}")
    (test-expression "+1/3i" "0+1/3i" "0+1/3i")
    (test-expression "+1/2i" "0+0.5i" "0+0.5i")
    (test-expression "779625/32258" 
                     "{number 779625/32258 \"24.1684233368466736933473866...\" decimal}"
                     "{number 779625/32258 \"24.1684233368466736933473866...\" decimal}")
    (test-expression "(exact? 1.5)" "true")
    (test-undefined-fn "(print (floor (sqrt 2)))" "print")
    
    (test-expression "(let ([f (lambda (x) x)]) f)" 
                     "(lambda (a1) ...)"
                     "(lambda (a1) ...)")
    (test-expression ",1"
                       "unquote: misuse of a comma or unquote, not under a quasiquoting backquote")
    
    (test-expression "(list 1)" 
                     "(list 1)"
                     "(list 1)")
    (test-expression "(car (list))" "car: expects a pair, given empty")
    (test-undefined-var "argv")
    
    (test-undefined-fn "(define-syntax app syntax-case)" "define-syntax")
    
    (test-expression "#lang racket"
                     "read: #lang not enabled in the current context"
                     "read: #lang not enabled in the current context")
    (test-expression "(define (f)\n(+ (raise-user-error 'a \"b\")))\n(if (zero? (random 1)) (void) (set! f void))\n(f)"
                       "define: expected at least one variable after the function name, but found none"
                     #rx"define: expected at least one variable after the function name, but found none")))



;                                                                           
;                                                                           
;                                                                           
;                ;;;;                                                  ;;;; 
;                ;;;;                                                  ;;;; 
;  ;;;;;;;    ;;;;;;; ;;;  ;;; ;;;;;;;  ;;;; ;;;    ;;;;;   ;;;     ;;;;;;; 
;  ;;;;;;;;  ;;;;;;;; ;;;  ;;; ;;;;;;;; ;;;;;;;;;  ;;;;;;  ;;;;;   ;;;;;;;; 
;      ;;;; ;;;;;;;;;  ;;;;;;      ;;;; ;;;; ;;;; ;;;;;;; ;;;; ;; ;;;;;;;;; 
;   ;;;;;;; ;;;; ;;;;  ;;;;;;   ;;;;;;; ;;;; ;;;; ;;;;    ;;;;;;; ;;;; ;;;; 
;  ;;  ;;;; ;;;;;;;;;  ;;;;;;  ;;  ;;;; ;;;; ;;;; ;;;;;;; ;;;;;   ;;;;;;;;; 
;  ;;;;;;;;  ;;;;;;;;   ;;;;   ;;;;;;;; ;;;; ;;;;  ;;;;;;  ;;;;;;  ;;;;;;;; 
;   ;; ;;;;   ;;;;;;;   ;;;;    ;; ;;;; ;;;; ;;;;   ;;;;;   ;;;;    ;;;;;;; 
;                                                                           
;                                                                           
;                                                                           


(define (advanced)
  (parameterize ([language (list "How to Design Programs" #rx"Advanced Student(;|$)")])
    (check-top-of-repl)
    
    (generic-settings #t)
    (generic-output #t #t #t #f)
    (teaching-language-fraction-output)
    
    (test-hash-bang)
    (test-error-after-definition)
    
    (prepare-for-test-expression)
    
    (test-expression "'|.|" 
                     "'|.|"
                     "'|.|")
    (test-expression '("(equal? (list " image ") (list " image "))") 
                     "true"
                     "true")
    (test-expression "(define x 1)(define x 2)"
                     "x: this name was defined previously and cannot be re-defined"
                     "x: this name was defined previously and cannot be re-defined")
    
    (test-expression "(define-struct spider (legs))(make-spider 4)"
                     "(make-spider 4)"
                     "spider: this name was defined previously and cannot be re-defined")
    
    (test-expression "(sqrt -1)" 
                     "0+1i"
                     "0+1i")
    
    (test-undefined-var "class")
    
    (test-expression "shared" "shared: expected an open parenthesis before shared, but found none")
    
    (test-expression "(define (. x y) (* x y))"  "read: illegal use of \".\"")
    (test-expression "'(1 . 2)"  "read: illegal use of \".\"")
    
    (test-undefined-var "call/cc")
    
    (test-expression "(error 'a \"~a\" 1)" "a: ~a1")
    (test-expression "(error \"a\" \"a\")" "aa")
    
    (test-expression "(time 1)" 
                     #rx"cpu time: [0-9]+ real time: [0-9]+ gc time: [0-9]+\n1")
    
    (test-expression "true" 
                     "true"
                     "true")
    (test-undefined-var "mred^")
    (test-expression "(eq? 'a 'A)" 
                     "false"
                     "false")
    (test-expression "(set! x 1)"
                     "x: this variable is not defined"
                     "set!: cannot set variable before its definition: x")
    (test-expression "(define qqq 2) (set! qqq 1)" 
                     "(void)" 
                     "qqq: this name was defined previously and cannot be re-defined")
    (test-expression "(cond [(= 1 2) 3])" "cond: all question results were false")
    (test-expression "(cons 1 2)"  "cons: second argument must be a list, but received 1 and 2")
    (test-expression "(+ (list 1) 2)" "+: expects a number as 1st argument, given (list 1)")
    (test-expression "'(1)" 
                     "(list 1)"
                     "(list 1)")
    (test-expression "(define shrd (list 1)) (list shrd shrd)"
                     "(shared ((-1- (list 1))) (list -1- -1-))"
                     "shrd: this name was defined previously and cannot be re-defined")
    (test-expression "(local ((define x x)) 1)" "local variable used before its definition: x")
    (test-expression "(letrec ([x x]) 1)" "local variable used before its definition: x")
    (test-expression "(if 1 1 1)" "if: question result is not true or false: 1")
    (test-expression "(+ 1)" "1")
    
    (test-expression "1.0" "1" "1")
    (test-expression "#i1.0" "#i1.0" "#i1.0")
    (test-expression "4/3" 
                     "{number 4/3 \"1.3\" decimal}"
                     "{number 4/3 \"1.3\" decimal}")
    (test-expression "1/3" 
                     "{number 1/3 \"0.3\" decimal}"
                     "{number 1/3 \"0.3\" decimal}")
    (test-expression "-4/3" 
                     "{number -4/3 \"-1.3\" decimal}"
                     "{number -4/3 \"-1.3\" decimal}")
    (test-expression "-1/3" 
                     "{number -1/3 \"-0.3\" decimal}"
                     "{number -1/3 \"-0.3\" decimal}")
    (test-expression "3/2" 
                     "{number 3/2 \"1.5\" decimal}"
                     "{number 3/2 \"1.5\" decimal}")
    (test-expression "1/2" 
                     "{number 1/2 \"0.5\" decimal}"
                     "{number 1/2 \"0.5\" decimal}")
    (test-expression "-1/2" 
                     "{number -1/2 \"-0.5\" decimal}"
                     "{number -1/2 \"-0.5\" decimal}")
    (test-expression "-3/2" 
                     "{number -3/2 \"-1.5\" decimal}"
                     "{number -3/2 \"-1.5\" decimal}")
    (test-expression "+1/3i" 
                     "0+1/3i"
                     "0+1/3i")
    (test-expression "+1/2i" 
                     "0+0.5i"
                     "0+0.5i")
    (test-expression "779625/32258" 
                     "{number 779625/32258 \"24.1684233368466736933473866...\" decimal}"
                     "{number 779625/32258 \"24.1684233368466736933473866...\" decimal}")
    (test-expression "(exact? 1.5)" "true")
    (test-expression "(print (floor (sqrt 2)))" "#i1.0")
    
    (test-expression "(let ([f (lambda (x) x)]) f)" 
                     "(lambda (a1) ...)"
                     "(lambda (a1) ...)")
    (test-expression ",1"
                     "unquote: misuse of a comma or unquote, not under a quasiquoting backquote")
    
    (test-expression "(list 1)" 
                     "(list 1)"
                     "(list 1)")
    (test-expression "(car (list))" "car: expects a pair, given empty")
    (test-undefined-var "argv")
    
    (test-undefined-fn "(define-syntax app syntax-case)" "define-syntax")
    
    (test-expression "#lang racket"
                     "read: #lang not enabled in the current context"
                     "read: #lang not enabled in the current context")
    (test-expression "(define (f)\n(+ (raise-user-error 'a \"b\")))\n(if (zero? (random 1)) (void) (set! f void))\n(f)"
                     #rx"raise-user-error"
                     #rx"raise-user-error")))




(define (prepare-for-test-expression)
  (let ([drs (wait-for-drracket-frame)])
    (clear-definitions drs)
    (set-language #t)
    (sleep 1) ;; this shouldn't be neccessary....
    (do-execute drs)))

;; test-setting : (-> void) string string string -> void
;; opens the language dialog, runs `set-setting'
;; closes the language dialog, executes,
;; makes sure that `expression' produces
;; `result'. `set-setting' is expected to click around
;; in the language dialog.
;; `setting-name' is used in the error message when the test fails.
(define (test-setting set-setting setting-name expression result)
  (set-language #f)
  (set-setting)
  (let ([f (test:get-active-top-level-window)])
    (fw:test:button-push "OK")
    (wait-for-new-frame f))
  (let* ([drs (test:get-active-top-level-window)]
         [interactions (send drs get-interactions-text)])
    (clear-definitions drs)
    (type-in-definitions drs expression)
    (do-execute drs)
    (let* ([got (fetch-output/should-be-tested drs)])
      (unless (string=? result got)
        (eprintf "FAILED: ~s ~s ~s test\n expected: ~s\n      got: ~s\n"
                 (language) setting-name expression result got)))))

(define (test-hash-bang)
  (let* ([expression "#!/bin/sh\n1"]
         [result "1"]
         [drs (test:get-active-top-level-window)]
         [interactions (queue-callback (λ () (send drs get-interactions-text)))])
    (clear-definitions drs)
    (type-in-definitions drs expression)
    (do-execute drs)
    (let* ([got (fetch-output/should-be-tested drs)])
      (unless (string=? "1" got)
        (eprintf "FAILED: ~s ~a test\n expected: ~s\n     got: ~s\n"
                 (language) expression result got)))))

(define (fetch-output/should-be-tested . args)
  (regexp-replace (regexp
                   (string-append
                    (regexp-quote "")
                    "$"))
                  (apply fetch-output args)
                  ""))

(define (check-top-of-repl)
  (let ([drs (wait-for-drracket-frame)])
    (set-language #t)
    (with-handlers ([exn:fail? void])
      (fw:test:menu-select "Testing" "Disable tests"))
    (do-execute drs)
    (let* ([interactions (send drs get-interactions-text)]
           [short-lang (last (language))]
           [get-line (lambda (n) 
                       (queue-callback/res
                        (λ ()
                          (send interactions get-text 
                                (send interactions paragraph-start-position n)
                                (send interactions paragraph-end-position n)))))]
           [line0-expect (format "Welcome to DrRacket, version ~a [~a]." 
                                 (version:version)
                                 (system-type 'gc))]
           [line1-expect 
            (if (string? short-lang)
                (format "Language: ~a" short-lang)
                short-lang)]
           [line0-got (get-line 0)]
           [line1-got (get-line 1)])
      (unless (and (string=? line0-expect line0-got)
                   (if (string? short-lang)
                       (string=? line1-expect (substring line1-got
                                                         0
                                                         (min (string-length line1-expect)
                                                              (string-length line1-got))))
                       (regexp-match line1-expect line1-got)))
        (eprintf "expected lines: \n  ~a\n  ~a\ngot lines:\n  ~a\n  ~a\n" 
                 line0-expect line1-expect
                 line0-got line1-got)
        (eprintf "defs: ~s" (queue-callback/res (λ () (send (send drs get-definitions-text) get-text))))
        (error 'language-test.rkt "failed get top of repl test")))))


;; teaching-language-fraction-output
;; tests that the teaching languages properly handle repeating decimals
(define (teaching-language-fraction-output)
  (test-setting
   (lambda () (fw:test:set-radio-box! "Fraction Style" "Mixed fractions"))
   "Fraction Style -- Mixed fractions"
   "4/3"
   "{number 4/3 \"1 1/3\" mixed}")
  (test-setting
   (lambda () (fw:test:set-radio-box! "Fraction Style" "Repeating decimals"))
   "Fraction Style -- Repeating decimals"
   "4/3"
   "{number 4/3 \"1.3\" decimal}"))

;; plt-language-fraction-output : -> void
;; tests that the PLT languages properly handle repeating decimals
(define (plt-language-fraction-output)
  (test-setting
   (lambda () (fw:test:set-check-box! "Use decimal notation for rationals" #f))
   "Use decimal notation for rationals -- #f"
   "4/3 1/2 -1/3"
   "{number 4/3 \"1 1/3\" mixed}\n{number 1/2 \"1/2\" mixed}\n{number -1/3 \"- 1/3\" mixed}")
  (test-setting
   (lambda () (fw:test:set-check-box! "Use decimal notation for rationals" #t))
   "Use decimal notation for rationals -- #t"
   "4/3 1/2 -1/3"
   "{number 4/3 \"#e1.3\" decimal}\n{number 1/2 \"#e0.5\" decimal}\n{number -1/3 \"#e-0.3\" decimal}"))

(define (generic-settings false/true?)
  (test-setting
   (lambda () (fw:test:set-check-box! "Case sensitive" #t))
   "Case sensitive -- #t"
   "(eq? 'g 'G)" 
   (if false/true? "false" "#f"))
  (test-setting
   (lambda () (fw:test:set-check-box! "Case sensitive" #f))
   "Case sensitive -- #f"
   "(eq? 'g 'G)" 
   (if false/true? "true" "#t")))

(define (generic-output list? quasi-quote? has-sharing? has-print-printing?)
  (let* ([plain-print-style (if has-print-printing? "print" "write")]
         [drs (wait-for-drracket-frame)]
         [expression "(define x (list 2))\n(list x x)"]
         [set-output-choice
          (lambda (option show-sharing pretty?)
            (set-language #f)
            (fw:test:set-radio-box! "Output Style" option)
            (when (and has-sharing? show-sharing)
              (fw:test:set-check-box!
               "Show sharing in values"
               (if (eq? show-sharing 'on) #t #f)))
            (fw:test:set-check-box!
             "Insert newlines in printed values"
             pretty?)
            (let ([f (test:get-active-top-level-window)])
              (fw:test:button-push "OK")
              (wait-for-new-frame f)))]
         [shorten
          (lambda (str)
            (if ((string-length str) . <= . 45)
                str
                (string-append (substring str 0 45) "...")))]
         [test
          ;; answer must either be a string, or a procedure that accepts both zero and 1
          ;; argument. When the procedure accepts 1 arg, the argument is `got' and
          ;; the result must be a boolean indicating if the result was satisfactory.
          ;; if the procedure receives no arguments, it must return a descriptive string
          ;; for the error message
          (lambda (option show-sharing pretty? answer)
            (set-output-choice option show-sharing pretty?)
            (do-execute drs)
            (let ([got (fetch-output/should-be-tested drs)])
              (unless (if (procedure? answer)
                          (answer got)
                          (whitespace-string=? answer got))
                (eprintf "FAILED ~s ~a, sharing ~a pretty? ~a\n            got ~s\n       expected ~s\n"
                         (language) option show-sharing pretty?
                         (shorten got)
                         (if (procedure? answer) (answer) answer)))))])
    
    (clear-definitions drs)
    (type-in-definitions drs expression)
    
    (test plain-print-style 'off #t "((2) (2))")
    (when has-sharing?
      (test plain-print-style 'on #t "(#0=(2) #0#)"))
    (when quasi-quote?
      (test "Quasiquote" 'off #t "`((2) (2))")
      (when has-sharing?
        (test "Quasiquote" 'on #t "(shared ((-1- `(2))) `(,-1- ,-1-))")))
    
    (test "Constructor" 'off #t
          (if list?
              "(list (list 2) (list 2))"
              "(cons (cons 2 empty) (cons (cons 2 empty) empty))"))
    (when has-sharing?
      (test "Constructor" 'on #t
            (if list?
                "(shared ((-1- (list 2))) (list -1- -1-))"
                "(shared ((-1- (cons 2 empty))) (cons -1- (cons -1- empty)))")))
    
    ;; setup print / pretty-print difference
    (clear-definitions drs)
    (for-each fw:test:keystroke
              (string->list
               "(define (f n)\n(cond ((zero? n) (list))\n(else (cons n (f (- n 1))))))\n(f 200)"))
    (test "Constructor" #f #f
          (case-lambda
            [(x) (not (member #\newline (string->list x)))]
            [() "no newlines in result"]))
    (test "Constructor" #f #t
          (case-lambda
            [(x) (member #\newline (string->list x))]
            [() "newlines in result (may need to make the window smaller)"]))
    (test plain-print-style #f #f
          (case-lambda
            [(x) (not (member #\newline (string->list x)))]
            [() "no newlines in result"]))
    (test plain-print-style #f #t
          (case-lambda
            [(x) (member #\newline (string->list x))]
            [() "newlines in result (may need to make the window smaller)"]))))

(define re:out-of-sync
  (regexp
   "WARNING: Interactions window is out of sync with the definitions window\\."))

(define (test-error-after-definition)
  (let* ([drs (wait-for-drracket-frame)]
         [interactions-text (queue-callback/res (λ () (send drs get-interactions-text)))])
    (clear-definitions drs)
    (type-in-definitions drs "(define y 0) (define (f x) (/ x y)) (f 2)")
    (do-execute drs)
    (let ([last-para (queue-callback/res (λ () (send interactions-text last-paragraph)))])
      (type-in-interactions drs "y\n")
      (wait-for-computation drs)
      (let ([got
             (fetch-output/should-be-tested
              drs
              (queue-callback/res (λ () (send interactions-text paragraph-start-position (+ last-para 1))))
              (queue-callback/res
               (λ ()
                 (send interactions-text paragraph-end-position
                       (- (send interactions-text last-paragraph) 1)))))])
        (unless (equal? got "0")
          (eprintf "FAILED: test-error-after-definition failed, expected 0, got ~s\n" got))))))


;; test-expression : (union string 'xml 'image (listof (union string 'xml 'image)))
;;                   (union string regexp (string -> boolean)) 
;;                -> void
;; types an expression in the definitions window, executes it and tests the output
;; types an expression in the REPL and tests the output from the REPL.
(define (test-expression expression defs-expected [repl-expected defs-expected])
  (let* ([drs (wait-for-drracket-frame)]
         [interactions-text (queue-callback/res (λ () (send drs get-interactions-text)))]
         [definitions-text (queue-callback/res (λ () (send drs get-definitions-text)))]
         [handle-insertion
          (lambda (item)
            (cond
              [(eq? item 'image)
               (use-get/put-dialog 
                (lambda () (fw:test:menu-select "Insert" "Insert Image..."))
                (simplify-path (build-path (collection-path "icons") "recycle.png")))]
              [(string? item)
               (insert-in-definitions drs item)]
              [(eq? item 'xml)
               (fw:test:menu-select "Insert" "Insert XML Box")
               (for-each fw:test:keystroke (string->list "<a><b>"))]
              [else (error 'handle-insertion "unknown thing to insert ~s" item)]))]
         [check-expectation
          (lambda (expected got)
            (cond
              [(string? expected)
               (whitespace-string=? expected got)]
              [(regexp? expected)
               (regexp-match expected got)]
              [(procedure? expected)
               (expected got)]))]
         [make-err-msg
          (lambda (expected)
            (cond
              [(string? expected)
               "FAILED: ~s ~s expected ~s to produce:\n  ~s\ngot:\n  ~s\ninstead\n"]
              [(regexp? expected)
               "FAILED: ~s ~s expected ~s to match ~s, got ~s instead\n"]
              [(procedure? expected)
               "FAILED: ~s ~s expected ~s to pass predicate ~s, got ~s\n"]))])
    (clear-definitions drs)
    (insert-in-definitions drs (defs-prefix))
    (cond
      [(pair? expression) (for-each handle-insertion expression)]
      [else (handle-insertion expression)])
    (do-execute drs)
    
    (let ([got
           (fetch-output
            drs
            (queue-callback/res (λ () (send interactions-text paragraph-start-position 2)))
            (queue-callback/res
             (λ ()
               (send interactions-text paragraph-end-position
                     (- (send interactions-text last-paragraph) 1)))))])
      (when (regexp-match re:out-of-sync got)
        (error 'text-expression "got out of sync message"))
      (unless (check-expectation defs-expected got)
        (eprintf (make-err-msg defs-expected)
                 'definitions (language) expression defs-expected got)))
    
    (let ([dp (defs-prefix)])
      (queue-callback/res
       (λ ()
         ;; select all except the defs-prefix
         (send definitions-text set-position
               (string-length dp)
               (send definitions-text last-position))
         
         (send definitions-text copy)
         (send interactions-text set-position
               (send interactions-text last-position)
               (send interactions-text last-position))
         (send interactions-text paste))))
    
    (let ([last-para (queue-callback/res (λ () (send interactions-text last-paragraph)))])
      (alt-return-in-interactions drs)
      (wait-for-computation drs)
      (let ([got
             (fetch-output
              drs
              (queue-callback/res
               (λ ()
                 (send interactions-text paragraph-start-position (+ last-para 1))))
              (queue-callback/res
               (λ ()
                 (send interactions-text paragraph-end-position
                       (- (send interactions-text last-paragraph) 1)))))])
        (when (regexp-match re:out-of-sync got)
          (error 'text-expression "got out of sync message"))
        (unless (check-expectation repl-expected got)
          (eprintf (make-err-msg repl-expected)
                   'interactions
                   (language)
                   expression repl-expected got))))))

(define (test-undefined-var id)
  (test-expression id (format "~a: this variable is not defined" id)))

(define (test-undefined-fn exp id)
  (test-expression exp (format "~a: this function is not defined" id)))

(define-syntax (go stx)
  (syntax-case stx ()
    [(_ arg)
     (identifier? (syntax arg))
     (syntax (begin (flush-output)
                    (printf ">> starting ~a\n" 'arg)
                    (flush-output)
                    (arg)
                    (flush-output)
                    (printf ">> finished ~a\n" 'arg)
                    (flush-output)))]))

(define (run-test)
  (go module-lang)
  (go pretty-big)
  (go r5rs)
  (go beginner)
  (go beginner/abbrev)
  (go intermediate)
  (go intermediate/lambda)
  (go advanced)
  )

(fire-up-drracket-and-run-tests run-test)
