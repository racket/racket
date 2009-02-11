(require "test-harness.ss"
         scheme/unit)

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

(test-syntax-error "f not defined in unit exporting sig3"
  (unit (import) (export sig3 sig4)
        (define a #t)
        (define g zero?)
        (define (b t) (if t 3 0))))

(test-runtime-error exn:fail:contract? "x exported by unit1 not a number"
  (invoke-unit unit1))
(test-runtime-error exn:fail:contract? "x exported by unit1 not a number"
  (invoke-unit (compound-unit (import) (export)
                              (link (((S1 : sig1)) unit1)
                                    (() unit2 S1)))))
(test-runtime-error exn:fail:contract? "a provided by anonymous unit not a number"
  (invoke-unit (compound-unit (import) (export)
                              (link (((S3 : sig3) (S4 : sig4))
                                     (unit (import) (export sig3 sig4)
                                           (define a #t)
                                           (define f add1)
                                           (define g zero?)
                                           (define (b t) (if t 3 0))))
                                    (() unit3 S3 S4)))))

(test-runtime-error exn:fail:contract? "g provided by anonymous unit returns the wrong value"
  (invoke-unit (compound-unit (import) (export)
                              (link (((S3 : sig3) (S4 : sig4))
                                     (unit (import) (export sig3 sig4)
                                           (define a 3)
                                           (define f add1)
                                           (define g values)
                                           (define (b t) (if t 3 0))))
                                    (() unit3 S3 S4)))))

(test-runtime-error exn:fail:contract? "unit4 misuses function b"
  (invoke-unit (compound-unit (import) (export)
                              (link (((S3 : sig3) (S4 : sig4))
                                     (unit (import) (export sig3 sig4)
                                           (define a 3)
                                           (define f add1)
                                           (define g zero?)
                                           (define (b t) (if t 3 0))))
                                    (() unit4 S3 S4)))))

(test-runtime-error exn:fail:contract? "unit5 provides bad value for d"
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

(test-runtime-error exn:fail:contract? "unit7 reexports x with different (wrong) contract"
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

(test-runtime-error exn:fail:contract? "unit8 misuses f from internal unit"
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

(test-runtime-error exn:fail:contract? "unit9-1 provides wrong value for function f"
  (invoke-unit unit9))

(define-values/invoke-unit
  (unit
   (import) (export sig2)
   (define f values))
  (import)
  (export sig2))

(test-runtime-error exn:fail:contract? "top-level misuses f"
  (f #t))

(define-unit unit10
  (import sig1 sig2) (export)
  (if (zero? x)
      (f 3)
      (f #t)))

(let ()
  (define x 0)
  (define f (lambda (x) #t))
  (test-runtime-error exn:fail:contract? "top-level (via anonymous unit) provides improper f"
    (invoke-unit unit10 (import sig1 sig2))))

(let ()
  (define x 1)
  (define f values)
  (test-runtime-error exn:fail:contract? "unit10 misuses f from top-level"
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
  (test-runtime-error exn:fail:contract? "unit11 provides improper f"
    (f 3))
  (test-runtime-error exn:fail:contract? "top-level misuses f"
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
  (test-runtime-error exn:fail:contract? "unit19 provides bad f"
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
  (test-runtime-error exn:fail:contract? "unit23 provides f with no protection into a bad context"
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
  (test-runtime-error exn:fail:contract? "unit28-1 broke contract on f"
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
  (test-runtime-error exn:fail:contract? "unit17 misuses f"
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
  (test-runtime-error exn:fail:contract? "unit32 provides f with no protection into bad context"
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
  (test-runtime-error exn:fail:contract? "unit37-1 broke contract on f"
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
  (test-runtime-error exn:fail:contract? "unit38 allowed f to flow into uncontracted bad context"
    (invoke-unit unit39)))
(let ()
  (define-compound-unit unit40
    (import)
    (export)
    (link [((S : sig8)) unit19]
          [() unit38 S]))
  (test-runtime-error exn:fail:contract? "unit38 allowed f to flow into uncontracted bad context"
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
  (test-runtime-error exn:fail:contract? "unit17 misuses f"
    (invoke-unit unit42)))
(let ()
  (define-compound-unit unit43
    (import)
    (export)
    (link [((S : sig7)) unit18]
          [() unit41 S]))
  (test-runtime-error exn:fail:contract? "unit17 misuses f"
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
  (test-runtime-error exn:fail:contract? "unit17 misuses f"
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
  (test-runtime-error exn:fail:contract? "unit17 misuses f"
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
  (test-runtime-error exn:fail:contract? "unit47 provided bad f"
    (invoke-unit unit48)))
(let ()
  (define-compound-unit unit49
    (import)
    (export)
    (link [((S : sig8)) unit47]
          [() unit17 S]))
  (test-runtime-error exn:fail:contract? "unit17 misuses f"
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
  (test-runtime-error exn:fail:contract? "unit19 provides bad f"
    (invoke-unit unit51)))
(let ()
  (define-compound-unit unit52
    (import)
    (export)
    (link [((S : sig7)) unit50]
          [() unit16 S]))
  (test-runtime-error exn:fail:contract? "unit50 provides unprotected f into bad context"
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
  (test-runtime-error exn:fail:contract? "unit19 provides bad f"
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
  (test-runtime-error exn:fail:contract? "unit55-1 misuses f"
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
(test-runtime-error exn:fail:contract? "m2 broke the contract on U@ (string, not symbol)" (m2:w))
(test-runtime-error exn:fail:contract? "m1 broke the contract on U@ (number, not string)" (m2:v))

(test-syntax-error "no y in sig1"
  (unit/c (import (sig1 [y number?]))
          (export)))
(test-syntax-error "two xs for sig1"
  (unit/c (import)
          (export (sig1 [x string?] [x number?]))))
(test-syntax-error "no sig called faux^, so import description matching fails"
  (unit/c (import faux^) (export)))

(test-runtime-error exn:fail:contract? "unit bad-export@ does not export sig1"
  (let ()
    (define/contract bad-export@
      (unit/c (import) (export sig1))
      (unit (import) (export)))
    bad-export@))

(test-runtime-error exn:fail:contract? "contract on bad-import@ does not export sig1"
  (let ()
    (define/contract bad-import@
      (unit/c (import) (export))
      (unit (import sig1) (export) (+ x 1)))
    bad-import@))
            