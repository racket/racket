#lang racket/load

(require "test-harness.rkt"
         scheme/unit
         scheme/contract)

(define temp-unit-blame-re "\\(unit temp[0-9]*\\)")
(define top-level "top-level")

(define (match-blame re msg)
  (or (regexp-match? (format "blaming: ~a" re) msg)
      (regexp-match? (format "broke its contract:.*blaming: ~a" re) msg)))

(define (match-obj re msg)
  (or (regexp-match? (format "~a: contract violation" re) msg)
      (regexp-match? (format "~a: broke its contract" re) msg)))

(define (get-ctc-err msg)
  (cond
    [(regexp-match  #rx"contract violation\n *([^\n]*)\n" msg)
     =>
     (λ (x) (cadr x))]
    [(regexp-match #rx"broke its contract\n *([^\n]*)\n" msg)
     =>
     (lambda (x) (cadr x))]
    [else (error 'test-contract-error
                 (format "no specific error in message: \"~a\"" msg))]))

(define-syntax-rule (test-contract-error blame obj err expr)
  (test-contract-error/regexp
   (regexp-quote blame) (regexp-quote obj) (regexp-quote err)
   expr))

(define-syntax test-contract-error/regexp
  (syntax-rules ()
    ((_ blame obj err expr)
     (with-handlers ((exn:fail:contract?
                      (lambda (exn)
                        (let ([msg (exn-message exn)])
                          (cond
                            [(not (match-blame blame msg))
                             (error 'test-contract-error
                                    "blame \"~a\" not found in:\n\"~a\""
                                     blame msg)]
                            [(not (match-obj obj msg))
                             (error 'test-contract-error
                                    "object \"~a\" not found in:\n\"~a\""
                                    obj msg)]
                            [else 
                             (printf "got expected contract error \"~a\" on ~a blaming ~a: ok\n\t\"~a\"\n\n" 
                                     err obj blame (get-ctc-err msg))])))))
       expr
       (error 'test-contract-error 
              "expected contract error \"~a\" on ~a, got none"
              err 'expr)))))

(define-signature sig1
  ((contracted [x number?])))
(define-signature sig2
  ((contracted [f (-> number? number?)])))
(define-signature sig3 extends sig2
  ((contracted [g (-> number? boolean?)])))
(define-signature sig4
  ((contracted [a number?] [b (-> boolean? number?)])))
(define-signature sig5
  ((contracted [c string?])
   (contracted [d symbol?])))

(define-unit unit1
  (import)
  (export sig1)
  (define x #f))

(define-unit unit2
  (import sig1)
  (export sig2)
  (define (f n) x))

(define-unit unit3
  (import sig3 sig4)
  (export)

  (b (g a)))

(define-unit unit4
  (import sig3 sig4)
  (export)

  (g (b a)))

(define-unit unit5
  (import)
  (export sig5)

  (define-values (c d) (values "foo" 3)))

(test-syntax-error "misuse of contracted"
  contracted)
(test-syntax-error "invalid forms after contracted in signature"
  (define-signature x ((contracted x y))))
(test-syntax-error "identifier not first part of pair after contracted in signature"
  (define-signature x ((contracted [(-> number? number?) x]))))

(test-syntax-error "identifier h? not bound anywhere"
  (module h?-test scheme
    (define-signature s^
      ((define-values (f?) (values number?))
       (define-syntaxes (g?) (make-rename-transformer #'number?))
       (contracted [f (-> f? (and/c g? h?))])))))

(test-syntax-error "f not defined in unit exporting sig3"
  (unit (import) (export sig3 sig4)
        (define a #t)
        (define g zero?)
        (define (b t) (if t 3 0))))

(test-contract-error "(unit unit1)" "x" "not a number"
  (invoke-unit unit1))

(test-contract-error "(unit unit1)" "x"  "not a number"
  (invoke-unit (compound-unit (import) (export)
                              (link (((S1 : sig1)) unit1)
                                    (() unit2 S1)))))
(test-contract-error/regexp temp-unit-blame-re "a" "not a number"
  (invoke-unit (compound-unit (import) (export)
                              (link (((S3 : sig3) (S4 : sig4))
                                     (unit (import) (export sig3 sig4)
                                           (define a #t)
                                           (define f add1)
                                           (define g zero?)
                                           (define (b t) (if t 3 0))))
                                    (() unit3 S3 S4)))))

(test-contract-error/regexp temp-unit-blame-re "g" "not a boolean"
  (invoke-unit (compound-unit (import) (export)
                              (link (((S3 : sig3) (S4 : sig4))
                                     (unit (import) (export sig3 sig4)
                                           (define a 3)
                                           (define f add1)
                                           (define g values)
                                           (define (b t) (if t 3 0))))
                                    (() unit3 S3 S4)))))

(test-contract-error "(unit unit4)" "b" "not a boolean"
  (invoke-unit (compound-unit (import) (export)
                              (link (((S3 : sig3) (S4 : sig4))
                                     (unit (import) (export sig3 sig4)
                                           (define a 3)
                                           (define f add1)
                                           (define g zero?)
                                           (define (b t) (if t 3 0))))
                                    (() unit4 S3 S4)))))

(test-contract-error "(unit unit5)" "d" "not a symbol"
  (invoke-unit unit5))

(define-unit unit6
  (import)
  (export sig1)
  (define-unit unit6-1
    (import)
    (export sig1)
    (define x 3))
  (define-values/invoke-unit unit6-1
    (import)
    (export sig1)))

(invoke-unit unit6)

(define-signature sig6
  ((contracted [x boolean?])))

(define-unit unit7
  (import)
  (export sig6)
  (define-unit unit7-1
    (import)
    (export sig1)
    (define x 3))
  (define-values/invoke-unit unit7-1
    (import)
    (export sig1)))

(test-contract-error "(unit unit7)" "x" "not a boolean"
  (invoke-unit unit7))

(define-unit unit8
  (import)
  (export)
  (define-unit unit8-1
    (import)
    (export sig2)
    (define f values))
  (define-values/invoke-unit unit8-1
    (import)
    (export sig2))
  (f #t))

(test-contract-error "(unit unit8)" "f" "not a number"
  (invoke-unit unit8))

(define-unit unit9
  (import)
  (export)
  (define-unit unit9-1
    (import)
    (export sig2)
    (define f zero?))
  (define-values/invoke-unit unit9-1
    (import)
    (export sig2))
  (f 3))

(test-contract-error "(unit unit9-1)" "f" "not a number"
  (invoke-unit unit9))

(define-values/invoke-unit
  (unit
   (import) (export sig2)
   (define f values))
  (import)
  (export sig2))

(test-contract-error top-level "f" "not a number"
  (f #t))

(define-unit unit10
  (import sig1 sig2) (export)
  (if (zero? x)
      (f 3)
      (f #t)))

(let ()
  (define x 0)
  (define f (lambda (x) #t))
  (test-contract-error "(unit u)" "f" "not a number"
    (invoke-unit unit10 (import sig1 sig2))))

(let ()
  (define x 1)
  (define f values)
  (test-contract-error "(unit unit10)" "f" "not a number"
    (invoke-unit unit10 (import sig1 sig2))))

;; testing that contracts from extended signatures are checked properly
(define-unit unit11
  (import) (export sig3)
  (define (f n) #t)
  (define (g n) 3))

(let ()
  (define-values/invoke-unit unit11
    (import)
    (export sig3))
  (test-contract-error "(unit unit11)" "f" "not a number"
    (f 3))
  (test-contract-error top-level "f" "not a number"
    (f #t)))

;; unit/new-import-export tests

(define-signature sig7 (f))
(define-signature sig8 ((contracted [f (-> number? number?)])))
(define-signature sig9 ((contracted [f (-> number? number?)])))

;; All units that play nicely
(define-unit unit12
  (import sig7)
  (export)
  (f 3))
(define-unit unit13
  (import sig8)
  (export)
  (f 3))
(define-unit unit14
  (import)
  (export sig7)
  (define f (λ (n) 3)))
(define-unit unit15
  (import)
  (export sig8)
  (define f (λ (n) 3)))

;; All units that don't play nicely (or won't after converted)
(define-unit unit16
  (import sig7)
  (export)
  (f #t))
(define-unit unit17
  (import sig8)
  (export)
  (f #t))
(define-unit unit18
  (import)
  (export sig7)
  (define f (λ (n) #t)))
(define-unit unit19
  (import)
  (export sig8)
  (define f (λ (n) #t)))

;; Converting units without internal contract violations

;; uncontracted import -> contracted import
(define-unit/new-import-export unit20
  (import sig8)
  (export)
  (() unit12 sig7))
(let ()
  (define-compound-unit unit21
    (import)
    (export)
    (link [((S : sig8)) unit15]
          [() unit20 S]))
  (invoke-unit unit21))
(let ()
  (define-compound-unit unit22
    (import)
    (export)
    (link [((S : sig8)) unit19]
          [() unit20 S]))
  (test-contract-error "(unit unit19)" "f" "not a number"
    (invoke-unit unit22)))

;; contracted import -> uncontracted import
(define-unit/new-import-export unit23
  (import sig7)
  (export)
  (() unit13 sig8))
(let ()
  (define-compound-unit unit24
    (import)
    (export)
    (link [((S : sig7)) unit14]
          [() unit23 S]))
  (invoke-unit unit24))
(let ()
  (define-compound-unit unit25
    (import)
    (export)
    (link [((S : sig7)) unit18]
          [() unit23 S]))
  (test-contract-error "(unit unit23)" "f" "not a number"
    (invoke-unit unit25)))

;; contracted import -> contracted import
(define-unit/new-import-export unit26
  (import sig9)
  (export)
  (() unit13 sig8))
(let ()
  (define-unit unit27-1
    (import)
    (export sig9)
    (define (f n) 3))
  (define-compound-unit unit27-2
    (import)
    (export)
    (link [((S : sig9)) unit27-1]
          [() unit26 S]))
  (invoke-unit unit27-2))
(let ()
  (define-unit unit28-1
    (import)
    (export sig9)
    (define (f n) #f))
  (define-compound-unit unit28-2
    (import)
    (export)
    (link [((S : sig9)) unit28-1]
          [() unit26 S]))
  (test-contract-error "(unit unit28-1)" "f" "not a number"
    (invoke-unit unit28-2)))

;; uncontracted export -> contracted export
(define-unit/new-import-export unit29
  (import)
  (export sig8)
  ((sig7) unit14))
(let ()
  (define-compound-unit unit30
    (import)
    (export)
    (link [((S : sig8)) unit29]
          [() unit13 S]))
  (invoke-unit unit30))
(let ()
  (define-compound-unit unit31
    (import)
    (export)
    (link [((S : sig8)) unit29]
          [() unit17 S]))
  (test-contract-error "(unit unit17)" "f" "not a number"
    (invoke-unit unit31)))

;; contracted export -> uncontracted export
(define-unit/new-import-export unit32
  (import)
  (export sig7)
  ((sig8) unit15))
(let ()
  (define-compound-unit unit33
    (import)
    (export)
    (link [((S : sig7)) unit32]
          [() unit14 S]))
  (invoke-unit unit33))
(let ()
  (define-compound-unit unit34
    (import)
    (export)
    (link [((S : sig7)) unit32]
          [() unit16 S]))
  (test-contract-error "(unit unit32)" "f" "not a number"
    (invoke-unit unit34)))

;; contracted export -> contracted export
(define-unit/new-import-export unit35
  (import)
  (export sig9)
  ((sig8) unit15))
(let ()
  (define-unit unit36-1
    (import sig9)
    (export)
    (f 3))
  (define-compound-unit unit36-2
    (import)
    (export)
    (link [((S : sig9)) unit35]
          [() unit36-1 S]))
  (invoke-unit unit36-2))
(let ()
  (define-unit unit37-1
    (import sig9)
    (export)
    (f #f))
  (define-compound-unit unit37-2
    (import)
    (export)
    (link [((S : sig9)) unit35]
          [() unit37-1 S]))
  (test-contract-error "(unit unit37-1)" "f" "not a number"
    (invoke-unit unit37-2)))

;; Converting units with internal contract violations

;; uncontracted import -> contracted import
(define-unit/new-import-export unit38
  (import sig8)
  (export)
  (() unit16 sig7))
(let ()
  (define-compound-unit unit39
    (import)
    (export)
    (link [((S : sig8)) unit15]
          [() unit38 S]))
  (test-contract-error "(unit unit38)" "f" "not a number"
    (invoke-unit unit39)))
(let ()
  (define-compound-unit unit40
    (import)
    (export)
    (link [((S : sig8)) unit19]
          [() unit38 S]))
  (test-contract-error "(unit unit38)" "f" "not a number"
    (invoke-unit unit40)))

;; contracted import -> uncontracted import
(define-unit/new-import-export unit41
  (import sig7)
  (export)
  (() unit17 sig8))
(let ()
  (define-compound-unit unit42
    (import)
    (export)
    (link [((S : sig7)) unit14]
          [() unit41 S]))
  (test-contract-error "(unit unit17)" "f" "not a number"
    (invoke-unit unit42)))
(let ()
  (define-compound-unit unit43
    (import)
    (export)
    (link [((S : sig7)) unit18]
          [() unit41 S]))
  (test-contract-error "(unit unit17)" "f" "not a number"
    (invoke-unit unit43)))

;; contracted import -> contracted import
(define-unit/new-import-export unit44
  (import sig9)
  (export)
  (() unit17 sig8))
(let ()
  (define-unit unit45-1
    (import)
    (export sig9)
    (define (f n) 3))
  (define-compound-unit unit45-2
    (import)
    (export)
    (link [((S : sig9)) unit45-1]
          [() unit44 S]))
  (test-contract-error "(unit unit17)" "f" "not a number"
    (invoke-unit unit45-2)))
(let ()
  (define-unit unit46-1
    (import)
    (export sig9)
    (define (f n) #t))
  (define-compound-unit unit46-2
    (import)
    (export)
    (link [((S : sig9)) unit46-1]
          [() unit44 S]))
  (test-contract-error "(unit unit17)" "f" "not a number"
    (invoke-unit unit46-2)))

;; uncontracted export -> contracted export
(define-unit/new-import-export unit47
  (import)
  (export sig8)
  ((sig7) unit18))
(let ()
  (define-compound-unit unit48
    (import)
    (export)
    (link [((S : sig8)) unit47]
          [() unit13 S]))
  (test-contract-error "(unit unit47)" "f" "not a number"
    (invoke-unit unit48)))
(let ()
  (define-compound-unit unit49
    (import)
    (export)
    (link [((S : sig8)) unit47]
          [() unit17 S]))
  (test-contract-error "(unit unit17)" "f" "not a number"
    (invoke-unit unit49)))

;; contracted import -> uncontracted import
(define-unit/new-import-export unit50
  (import)
  (export sig7)
  ((sig8) unit19))
(let ()
  (define-compound-unit unit51
    (import)
    (export)
    (link [((S : sig7)) unit50]
          [() unit12 S]))
  (test-contract-error "(unit unit19)" "f" "not a number"
    (invoke-unit unit51)))
(let ()
  (define-compound-unit unit52
    (import)
    (export)
    (link [((S : sig7)) unit50]
          [() unit16 S]))
  (test-contract-error "(unit unit50)" "f" "not a number"
    (invoke-unit unit52)))

;; contracted export -> contracted export
(define-unit/new-import-export unit53
  (import)
  (export sig9)
  ((sig8) unit19))
(let ()
  (define-unit unit54-1
    (import sig9)
    (export)
    (f 3))
  (define-compound-unit unit54-2
    (import)
    (export)
    (link [((S : sig9)) unit53]
          [() unit54-1 S]))
  (test-contract-error "(unit unit19)" "f" "not a number"
    (invoke-unit unit54-2)))
(let ()
  (define-unit unit55-1
    (import sig9)
    (export)
    (f #t))
  (define-compound-unit unit55-2
    (import)
    (export)
    (link [((S : sig9)) unit53]
          [() unit55-1 S]))
  (test-contract-error "(unit unit55-1)" "f" "not a number"
    (invoke-unit unit55-2)))

(module m1 scheme
  (define-signature foo^ (x))
  (define-signature bar^ (y))
  (provide foo^ bar^)
  
  (define-unit U@
    (import foo^)
    (export bar^)
    (define (y s)
      (if (eq? s 'bork)
          3
          (string-append (symbol->string s) " " (if (x 3) "was true on 3" "was not true on 3")))))
  (provide/contract [U@ (unit/c (import (foo^ [x (-> number? boolean?)]))
                                (export (bar^ [y (-> symbol? string?)])))]))

(module m2 scheme
  (require 'm1)
  
  (define x zero?)
  (define-values/invoke-unit U@
    (import foo^)
    (export bar^))
  
  (define (z)
    (y 'a))
  (define (w)
    (y "foo"))
  (define (v)
    (y 'bork))
  
  (provide z w v))

(require (prefix-in m2: 'm2))

(m2:z)
(test-contract-error "m2" "U@" "not a symbol" (m2:w))
(test-contract-error "m1" "U@" "not a string" (m2:v))

(test-syntax-error "no y in sig1"
  (unit/c (import (sig1 [y number?]))
          (export)))
(test-syntax-error "two xs for sig1"
  (unit/c (import)
          (export (sig1 [x string?] [x number?]))))
(test-syntax-error "no sig called faux^, so import description matching fails"
  (unit/c (import faux^) (export)))

(test-contract-error "(definition bad-export@)" "bad-export@" "unit must export sig1"
  (let ()
    (define/contract bad-export@
      (unit/c (import) (export sig1))
      (unit (import) (export)))
    bad-export@))

(test-contract-error "(definition bad-import@)" "bad-import@" "contract must import sig1"
  (let ()
    (define/contract bad-import@
      (unit/c (import) (export))
      (unit (import sig1) (export) (+ x 1)))
    bad-import@))

(test-contract-error "(definition not-a-unit)" "not-a-unit" "not a unit"
  (let ()
    (define/contract not-a-unit
      (unit/c (import) (export))
      3)
    not-a-unit))

;; Adding a test to make sure that contracts can refer
;; to other parts of the signature.

(module m3 scheme
  (define-signature toy-factory^
    ((contracted
      [build-toys (-> integer? (listof toy?))]
      [repaint    (-> toy? symbol? toy?)]
      [toy?       (-> any/c boolean?)]
      [toy-color  (-> toy? symbol?)])))

  (define-unit simple-factory@
    (import)
    (export toy-factory^)

    (define-struct toy (color) #:transparent)

    (define (build-toys n)
      (for/list ([i (in-range n)])
        (make-toy 'blue)))

    (define (repaint t col)
      (make-toy col)))

  (provide toy-factory^ simple-factory@))

(module m4 scheme
  (define-signature foo^ (x? (contracted [f (-> x? boolean?)])))

  (define-unit U@
    (import)
    (export foo^)
    (define (x? x) #f)
    (define (f x) (x? x)))

  (define-values/invoke-unit/infer U@)
  
  (provide f x?))

(require (prefix-in m4: 'm4))

(test-contract-error "m4" "f" "not an x"
  (m4:f 3))

(require (prefix-in m3: 'm3))

(test-contract-error top-level "build-toys" "not a integer"
  (let ()
    (define-values/invoke-unit/infer m3:simple-factory@)
    (build-toys #f)))

(module m5 scheme
  (define-signature foo^ (f (contracted [x? (-> any/c boolean?)])))
  
  (define-unit U@
    (import)
    (export foo^)
    (define (x? n) (zero? n))
    (define (f x) (x? x)))
  
  (provide foo^)
  (provide/contract
   [U@
    (unit/c (import)
            (export (foo^ [f (-> x? boolean?)])))]))

(require (prefix-in m5: 'm5))

(define-values/invoke-unit m5:U@
  (import)
  (export (prefix m5: m5:foo^)))

(m5:f 0)

(test-contract-error top-level "U@" "not an x"
  (m5:f 3))

(let ()
  (define-signature foo^ (x? f))
  (define-signature bar^ ((contracted [x? (-> number? boolean?)]
                                      [f  (-> x? number?)])))
  (define-unit U@
    (import)
    (export bar^)
    (define x? zero?)
    (define f values))
  
  (define-unit/new-import-export V@
    (import)
    (export bar^)
    ((bar^) U@))
  
  (define-values/invoke-unit/infer V@)
  
  (f 0)
  (test-contract-error top-level "f" "not an x"
    (f 3)))

(let ()
  (define-signature foo^ ((contracted [x? (-> number? boolean?)]
                                      [f (-> x? number?)])))
  (define-signature bar^ (f (contracted [x? (-> any/c boolean?)])))
  (define-unit U@
    (import)
    (export foo^)
    (define x? zero?)
    (define f values))
  
  (define-unit/new-import-export V@
    (import)
    (export bar^)
    ((foo^) U@))
  
  (define-values/invoke-unit/infer V@)
  
  (f 0)
  (test-contract-error "(unit V@)" "f" "not an x"
    (f 3)))

(let ()
  (define-signature foo^ (x y))
  (define-unit/contract U@
    (import)
    (export (foo^ [x (-> number? number?)]))
    (define (x n) (zero? n))
    (define y 4))
  (define-unit V@
    (import foo^)
    (export)
    (x 4))
  (define-compound-unit/infer W@
    (import) (export) (link U@ V@))
  (define-values/invoke-unit/infer U@)
  y
  (test-contract-error top-level "U@" "not a number"
    (x #t))
  (test-contract-error "(unit U@)" "U@" "not a number"
    (x 3))
  (test-contract-error "(unit U@)" "U@" "not a number"
    (invoke-unit W@)))

(let ()
  (define-signature foo^ (x? f))
  (define-unit/contract U@
    (import)
    (export (foo^ [f (-> x? number?)]))
    (define (x? n) (or (= n 3)
                       (zero? n)))
    (define (f n) (if (= n 3) #t n)))
  (define-unit V@
    (import foo^)
    (export)
    (test-contract-error top-level "U@" "not an x"
      (f 2))
    (test-contract-error "(unit U@)" "U@" "not an number"
      (f 3)))
  (define-compound-unit/infer W@
    (import) (export) (link U@ V@))
  (define-values/invoke-unit/infer U@)
  (test-contract-error top-level "U@" "not an x"
    (f 4))
  (test-contract-error "(unit U@)" "U@" "not a number"
    (f 3))
  (invoke-unit W@))

(let ()
  (define-signature foo^
    ((contracted 
      [x? (-> number? boolean?)]
      [f (-> x? number?)])))
  
  (define-unit/contract foo@
    (import)
    (export (foo^ [x? (-> any/c boolean?)]))
    
    (define (x? n) (zero? n))
    (define (f x) 3))
  
  (define-values/invoke-unit/infer foo@)
  
  (f 0)
  (test-contract-error top-level "f" "not an x"
    (f 4))
  ;; This is a weird one.  The definition for foo@ has two conflicting
  ;; contracts.  Who gets blamed?  Still the top-level, since foo@ can't
  ;; get blamed for breaking its own contract.  In theory you could say
  ;; that perhaps the top-level shouldn't be blamed, and that it should
  ;; just be an "overriding" contract, but a) that won't really work and
  ;; b) what about other units that might link with foo@, that expect
  ;; the stronger contract?
  (test-contract-error top-level "x?" "not a number"
    (f #t)))

(let ()
  (define-signature student^
    ((struct/ctc student ([name string?] [id number?]))))
  (define-unit student@
    (import)
    (export student^)
    (define-struct student (name id)))
  (define-values/invoke-unit/infer student@)
  (make-student "foo" 3)
  (test-contract-error top-level "make-student" "not a string"
    (make-student 4 3))
  (test-contract-error top-level "student-id" "not a student"
    (student-id 'a)))

;; Test that prefixing doesn't cause issues.
(let ()
  (define-signature t^
    ((contracted (t? (any/c . -> . boolean?))
                 (make-t (-> t?)))))
  
  (define-unit t@
    (import)
    (export t^)
    (define-struct t ()))
  
  (define-signature s^ (new-make-t))
  
  (define-unit s@
    (import (prefix pre: t^))
    (export s^)
    (define new-make-t pre:make-t))
  
  (define c@ (compound-unit (import)
                            (export S)
                            (link [((T : t^)) t@]
                                  [((S : s^)) s@ T])))
  (define-values/invoke-unit c@ (import) (export s^))
  (new-make-t))

(displayln "tests passed")
