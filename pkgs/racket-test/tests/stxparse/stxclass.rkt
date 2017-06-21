#lang scheme/base
(require rackunit
         syntax/parse
         syntax/parse/debug
         (for-syntax scheme/base syntax/parse))

;; Testing stuff

(define-syntax-rule (stx-like? expr template)
  (equal? (stx->datum expr) 'template))

(define (stx->datum expr)
  (syntax->datum (datum->syntax #f expr)))

;; Syntax classes

(define-syntax-class one
  (pattern a))

(define-syntax-class two
  (pattern (a b)))

(define-syntax-class three
  (pattern (a b c)))

(define-syntax-class two-or-three/flat
  (pattern (a b))
  (pattern (a b c)))

(define-syntax-class two-or-three/tag
  #:attributes (a a.a a.b)
  (pattern a:two)
  (pattern a:three))

(define-syntax-class two-to-four/untagged
  #:attributes (a b)
  (pattern :two)
  (pattern :three)
  (pattern (a b c d)))

(define-syntax-class xs
  (pattern (x ...)))

(define-syntax-class pairs
  (pattern ((x y) ...)))

(define-syntax-class id-num
  (pattern (x n)
           #:declare x id
           #:declare n number))

(define-syntax-class id-string
  (pattern (x:id label:str)))

;; Test macros

(define-syntax-rule (test-sc-attrs name ([attr depth] ...))
  (test-case (format "~s" 'name)
    (let* ([r-attrs (syntax-class-attributes name)]
           [r-names (map car r-attrs)]
           [expected '((attr depth) ...)])
      (for ([ra r-names])
           (check-memq ra '(attr ...) "Unexpected attr returned"))
      (for ([a '(attr ...)])
           (check-memq a r-names "Expected attr not returned"))
      (for ([rec r-attrs])
           (let ([ex (assq (car rec) expected)])
             (check-equal? (cadr rec) (cadr ex) "Wrong depth returned"))))))

(define-simple-check (check-memq item items)
  (memq item items))

(define-syntax-rule (test-parse-sc sc stx ([attr depth form] ...))
  (test-case (format "~s" 'sc)
    (let* ([r (syntax-class-parse sc stx)]
           [r-attrs (for/list ([record r]) (vector-ref record 0))]
           [expected '([attr depth form] ...)])
      (for ([ra r-attrs])
        (check-memq ra '(attr ...) "Unexpected attr returned"))
      (for ([a '(attr ...)])
        (check-memq a r-attrs "Expected attr not returned"))
      (for ([rec r])
        (let ([ex (assq (vector-ref rec 0) expected)])
          (check-equal? (vector-ref rec 1) (cadr ex))
          (check-equal? (stx->datum (vector-ref rec 2)) (caddr ex)))))))

(define-syntax-rule (test-patterns pattern stx body ...)
  (test-case (format "~s" 'pattern)
             (syntax-parse stx [pattern body ... (void)])))

;; Tests

(begin ;; define tests
  (begin ;; test-suite "Syntax grammars"
    (begin ;; test-suite "sc attrs"
      (test-sc-attrs one ([a 0]))
      (test-sc-attrs two ([a 0] [b 0]))
      (test-sc-attrs three ([a 0] [b 0] [c 0]))
      (test-sc-attrs two-or-three/tag ([a 0] [a.a 0] [a.b 0]))
      (test-sc-attrs id-num ([x 0] [n 0])))
    (begin ;; test-suite "parse-sc"
      (test-parse-sc one #'1 ([a 0 1]))
      (test-parse-sc two #'(1 2) ([a 0 1] [b 0 2]))
      (test-parse-sc three #'(1 2 3) ([a 0 1] [b 0 2] [c 0 3]))
      (test-parse-sc two-or-three/tag #'(1 2 3)
                     ([a 0 (1 2 3)] [a.a 0 1] [a.b 0 2]))
      (test-parse-sc id-num #'(this 12)
                     ([x 0 this] [n 0 12]))
      (test-parse-sc id-string #'(that "here")
                     ([x 0 that] [label 0 "here"])))
    (begin ;; test-suite "with-patterns"
      (test-patterns (t:two-to-four/untagged ...) #'((1 2 3) (4 5) (6 7 8))
        (check-equal? (syntax->datum #'(t.a ...)) '(1 4 6)))
      (test-patterns (t:two-to-four/untagged ...) #'((1 2 3) (4 5) (6 7 8))
        (check-equal? (syntax->datum #'(t.b ...)) '(2 5 7)))
      (test-patterns ({~alt {~seq x:id v:nat} s:str} ...) #'(x 1 y 2 "whee" x 3)
        (check-equal? (stx->datum #'((x v) ...)) '((x 1) (y 2) (x 3)))
        (check-equal? (stx->datum #'(s ...)) '("whee")))
      (test-patterns ({~alt {~seq x:id v:nat} s:str} ...) #'(x 1 y 2 "whee" x 3)
        (check-equal? (stx->datum #'((x v) ...)) '((x 1) (y 2) (x 3)))
        (check-equal? (stx->datum #'(s ...)) '("whee")))
      (test-patterns ({~alt (~once 1)
                            (~once 2)
                            (~once 3)} ...)
                     #'(1 2 3)
        (void))
      (test-patterns ({~alt a:id b:nat c:str} ...) #'("one" 2 three)
        (check-equal? (stx->datum #'(a ...)) '(three))
        (check-equal? (stx->datum #'(b ...)) '(2))
        (check-equal? (stx->datum #'(c ...)) '("one"))
        (void))
      (test-patterns ({~alt (~once 1)
                            (~once 2)
                            (~once 3)
                            (~once x)
                            (~once y)
                            (~once w)} ...)
                     #'(1 2 3 x y z)
        (for ([s (syntax->list #'(x y w))]) (check-pred identifier? s))
        (check-equal? (sort 
                       (map symbol->string (stx->datum #'(x y w)))
                       string<?)
                      '("x" "y" "z"))
        (void))
      (test-patterns ({~alt x
                            (~once 1)
                            (~once 2)
                            (~once 3)} ...)
                     #'(1 2 3 x y z)
        (check-equal? (stx->datum #'(x ...)) '(x y z))
        (void))
      )))

(define-syntax-class bindings
  (pattern ((var:id e) ...)
           #:with vars #'(var ...)))

(define-syntax-class sorted
  (pattern (n:nat ...)
           #:fail-unless (sorted? (syntax->datum #'(n ...))) "not sorted"))

(define (sorted? ns)
  (define (loop ns min)
    (cond [(pair? ns)
           (and (<= min (car ns))
                (loop (cdr ns) (car ns)))]
          [(null? ns) #t]))
  (loop ns -inf.0))

(define-syntax-class Opaque
  #:opaque
  (pattern (a:id n:nat)))
(define-syntax-class Transparent
  (pattern (a:id n:nat)))

#|
(with-handlers ([exn? exn-message])
  (syntax-parse #'(0 1) [_:Opaque (void)]))
(with-handlers ([exn? exn-message])
  (syntax-parse #'(0 1) [_:Transparent (void)]))
|#

(syntax-parse #'(+) #:literals ([plus +])
  [(plus) (void)])


(define-syntax-class (Nat> n)
  #:description (format "Nat > ~s" n)
  (pattern x:nat #:fail-unless (> (syntax-e #'x) n) #f))
(syntax-parse #'(1 2 3)
  [(a:nat b0:nat c0:nat)
   #:with b #'b0
   #:declare b (Nat> (syntax-e #'a))
   #:with c #'c0
   #:declare c (Nat> (syntax-e #'b0))
   (void)])

(define-syntax-class (Nat-Between #:lo lo #:hi hi)
  #:description (format "~s <= Nat <= ~s" lo hi)
  (pattern x:nat #:when (<= lo (syntax-e #'x) hi)))
(syntax-parse #'5
  [(~var _ (Nat-Between #:lo 0 #:hi 10)) (void)])
