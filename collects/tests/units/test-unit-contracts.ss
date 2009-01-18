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