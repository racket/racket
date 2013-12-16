
; Test Racket's name inference

(load-relative "loadtest.rktl")

(require scheme/class)
(require scheme/unit)

(Section 'names)

(arity-test object-name 1 1)
(test #f object-name 0)
(test #f object-name 'hello)
(test #f object-name "hi")

(define (src-name? s)
  (and (symbol? s)
       (regexp-match ":[0-9]+.[0-9]+$" (symbol->string s))
       #t))

; Test ok when no name for proc
(test #t src-name? (object-name (lambda () 0)))
(test #t src-name? (object-name (case-lambda)))
(test #t src-name? (object-name (case-lambda [(x) 9])))
(test #t src-name? (object-name (case-lambda [(x) 9][(y z) 12])))

; Test constructs that don't provide a name
(test #t src-name? (object-name (let ([x (cons (lambda () 10) 0)]) (car x))))
(test #t src-name? (object-name (let ([x (let ([y (lambda (x) x)]) (y (lambda () 10)))]) x)))

; Test ok when name for proc
(define f (lambda () 0))
(define f2 (lambda (a) 0))
(define f3 (case-lambda))
(define f4 (case-lambda [(x) 9]))
(define f5 (case-lambda [(x) 9][(y z) 10]))

(test 'f object-name f)
(test 'f2 object-name f2)
(test 'f3 object-name f3)
(test 'f4 object-name f4)
(test 'f5 object-name f5)

; Test constructs that do provide a name
(test 'a object-name (let ([a (lambda () 0)]) a))
(test 'a object-name (let ([a (lambda () 0)]) (let ([b a]) b)))
(test 'b object-name (let* ([b (lambda () 0)]) b))
(test 'c object-name (letrec ([c (lambda () 0)]) c))
(test 'loop object-name (let loop () loop))

(test 'd object-name (let ([d (begin (lambda () x))]) d))
(test 'e object-name (let ([e (begin0 (lambda () x))]) e))

(test 'd2 object-name (let ([d2 (begin 7 (lambda () x))]) d2))
(test 'e2 object-name (let ([e2 (begin0 (lambda () x) 7)]) e2))

(test 'd3 object-name (let ([d3 (begin (cons 1 2) (lambda () x))]) d3))
(test 'e3 object-name (let ([e3 (begin0 (lambda () x) (cons 1 2))]) e3))

(test 'f object-name (let ([f (begin0 (begin (cons 1 2) (lambda () x)) (cons 1 2))]) f))

(test 'g1 object-name (let ([g1 (if (cons 1 2) (lambda () x) #f)]) g1))
(test 'g2 object-name (let ([g2 (if (negative? (car (cons 1 2))) #t (lambda () x))]) g2))

(test 'w object-name (let ([w (let ([x 5]) (lambda () x))]) w))
(test 'z object-name (let ([z (let ([x 5]) (cons 1 2) (lambda () x))]) z))

(set! f (lambda () 10))
(test 'f object-name f)

; Test class stuff ok when no name
(test #t src-name? (object-name (class object% (super-make-object))))
(test #t src-name? (object-name (interface ())))

; Test class stuff ok when name
(test 'c1 object-name (let ([c1 (class object% (super-make-object))]) c1))
(test 'i1 object-name (let ([i1 (interface ())]) i1))

; Test unit stuff ok when no name
(test #t src-name? (object-name (unit (import) (export))))
(test #t src-name? (object-name (compound-unit (import) (link) (export))))

; Test unit stuff ok when name
(test 'unit:u1 object-name (let ([u1 (unit (import) (export))]) u1))
(test 'unit:u2 object-name (let ([u2 (compound-unit (import) (export) (link))]) u2))

(test 'x object-name (invoke-unit
		       (unit (import) (export) (define x (lambda () 0)) x)))
(define-signature x2^ (x2))
(test 'x2 object-name (invoke-unit
		        (unit (import) (export x2^) (define x2 (lambda () 0)) x2)))

;; Use external name instead?:
(define-signature x3^ (x3))
(test 'x object-name (invoke-unit
		       (unit (import) (export (rename x3^ [x x3])) (define x (lambda () 0)) x)))

; Test case sensitivity
(parameterize ([read-case-sensitive #t])
  (test (string->symbol "Capital")
	object-name
	(eval (read (open-input-string "(let ([Capital (lambda () 10)]) Capital)"))))
  (test (string->symbol "CP")
	object-name
	(eval (read (open-input-string "(let () (define-struct CP (a)) make-CP)")))))


(err/rt-test (let ([unmentionable ((lambda (x #:a a) 1) 1 2)]) 5)
             (lambda (exn) (not (regexp-match? #rx"unmentionable" (exn-message exn)))))
(err/rt-test (let ([unmentionable ((lambda (x #:a a) 1) #:q 1 2)]) 5)
             (lambda (exn) (not (regexp-match? #rx"unmentionable" (exn-message exn)))))


(err/rt-test (let ([mentionable (let ()
                                  (define v 1)
                                  (lambda (x #:a a) v))])
               (mentionable 1 2))
             (lambda (exn) (regexp-match? #rx"mentionable" (exn-message exn))))
(err/rt-test (let ([mentionable (let ()
                                  (define v 1)
                                  (lambda (x #:a a) v))])
               (mentionable #:q 1 2))
             (lambda (exn) (regexp-match? #rx"mentionable" (exn-message exn))))

(syntax-test #'(let-syntax ([fail (lambda (stx)
                                    (raise-syntax-error 'fail 
                                                        (format "~s" (syntax-local-name))))])
                 (let ([unmentionable (let ()
                                        (fail)
                                        10)])
                   5))
             #rx"^(?!.*unmentionable)")

(report-errs)
