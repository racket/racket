#lang scheme/base

(require (planet "test.ss" ("schematics" "schemeunit.plt" 2 9))
         (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2 9))
         "sc.ss"
         "lib.ss"
         (for-syntax scheme/base "sc.ss" "lib.ss"))

;; Testing stuff

(define-namespace-anchor anchor)
(define tns (namespace-anchor->namespace anchor))
(define (teval expr) (eval expr tns))

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
  (union (pattern (a b))
         (pattern (a b c))))

(define-syntax-class two-or-three/tag
  (union (pattern a:two)
         (pattern a:three)))

(define-syntax-class two-to-four/untagged
  (union two
         three
         (pattern (a b c d))))

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
    (let* ([r-attrs (attrs-of name)]
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
    (let* ([r (parse-sc sc stx)]
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

(define-syntax-rule (test-patterns pattern stx . body)
  (test-case (format "~s" 'pattern)
             (with-patterns ([pattern stx]) . body)))

;; Tests

(define tests
  (test-suite "Syntax grammars"
    (test-suite "sc attrs"
      (test-sc-attrs one ([a 0]))
      (test-sc-attrs two ([a 0] [b 0]))
      (test-sc-attrs three ([a 0] [b 0] [c 0]))
      (test-sc-attrs two-or-three/tag ([a 0] [a.a 0] [a.b 0]))
      (test-sc-attrs id-num ([x 0] [x.datum 0] [n 0] [n.datum 0])))
    (test-suite "parse-sc"
      (test-parse-sc one #'1 ([a 0 1]))
      (test-parse-sc two #'(1 2) ([a 0 1] [b 0 2]))
      (test-parse-sc three #'(1 2 3) ([a 0 1] [b 0 2] [c 0 3]))
      (test-parse-sc two-or-three/tag #'(1 2 3)
                     ([a 0 (1 2 3)] [a.a 0 1] [a.b 0 2]))
      (test-parse-sc id-num #'(this 12)
                     ([x 0 this] [x.datum 0 this] [n 0 12] [n.datum 0 12]))
      (test-parse-sc id-string #'(that "here")
                     ([x 0 that] [x.datum 0 that]
                      [label 0 "here"] [label.datum 0 "here"])))
    (test-suite "with-patterns"
      (test-patterns (t:two-to-four/untagged ...) #'((1 2 3) (4 5) (6 7 8))
        (check-equal? (syntax->datum #'(t.a ...)) '(1 4 6)))
      (test-patterns (t:two-to-four/untagged ...) #'((1 2 3) (4 5) (6 7 8))
        (check-equal? (syntax->datum #'(t.b ...)) '(2 5 7)))
      (test-patterns ({{x:id v:nat} {s:str}} ...*) #'(x 1 y 2 "whee" x 3)
        (check-equal? (stx->datum #'((x v) ...)) '((x 1) (y 2) (x 3)))
        (check-equal? (stx->datum #'(s ...)) '("whee")))
      (test-patterns ({{x:id v:nat} {s:str}} ...*) #'(x 1 y 2 "whee" x 3)
        (check-equal? (stx->datum #'((x v) ...)) '((x 1) (y 2) (x 3)))
        (check-equal? (stx->datum #'(s ...)) '("whee")))
      (test-patterns ({{1} #:min 1 #:max 1
                       {2} #:min 1 #:max 1
                       {3} #:min 1 #:max 1} ...*)
                     #'(1 2 3)
        'ok)
      (test-patterns ({{a:id} {b:nat} {c:str}} ...*) #'("one" 2 three)
        (check-equal? (stx->datum #'(a ...)) '(three))
        (check-equal? (stx->datum #'(b ...)) '(2))
        (check-equal? (stx->datum #'(c ...)) '("one")))
      (test-patterns ({{1} #:min 1 #:max 1
                       {2} #:min 1 #:max 1
                       {3} #:min 1 #:max 1
                       {x} #:min 1 #:max 1
                       {y} #:min 1 #:max 1
                       {w} #:min 1 #:max 1} ...*)
                     #'(1 2 3 x y z)
        (for ([s (syntax->list #'(x ... y ... w ...))]) (check-pred identifier? s))
        (check-equal? (sort 
                       (map symbol->string (stx->datum #'(x ... y ... w ...)))
                       string<?)
                      '("x" "y" "z")))
      (test-patterns ({{x}
                       {1} #:min 1 #:max 1
                       {2} #:min 1 #:max 1
                       {3} #:min 1 #:max 1} ...*)
                     #'(1 2 3 x y z)
        (check-equal? (stx->datum #'(x ...)) '(x y z)))
      )))

(define-syntax (test-expr stx)
  (with-patterns ([(_ e:expr/local-expand) stx])
    #'(quote e.expanded)))

(define-syntax (convert-block stx)
  (with-patterns ([(_ . b:block/head-local-expand) stx])
    (with-patterns ([((_ svars srhs) ...) #'(b.sdef ...)]
                    [((_ vvars vrhs) ...) #'(b.vdef ...)])
      ;;(printf "here's the expanded block:\n~s\n" #'b.expanded-block)
      #'(letrec-syntaxes+values ((svars srhs) ...) ((vvars vrhs) ...)
          (begin b.expr ...)))))

(define-syntax (begin/defs stx)
  (with-patterns 
      ([(_ . b:internal-definitions) stx]
       [((_ svars srhs) ...) #'(b.sdef ...)]
       [((_ (vvar ...) bleh) ...) #'(b.vdef ...)]
       [(expr ...)
        (for/list ([form (syntax->list #'(b.expanded ...))])
          (syntax-parse form
            [dv:define-values-form
             #'(set!-values (dv.var ...) dv.rhs)]
            [ds:define-syntaxes-form
             #'(void)]
            [e
             #'e]))])
    #'(letrec-syntaxes+values 
          ((svars srhs) ...)
          (((vvar ...) (let ((vvar #f) ...) (values vvar ...))) ...)
        (begin expr ...))))

(define-syntax (begin/defs* stx)
  (with-patterns 
      ([(_ . b:internal-definitions) stx]
       [((_ svars srhs) ...) #'(b.sdef ...)]
       [(head ... last) #'(b.expanded ...)]
       [((preclause ...) ...)
        (for/list ([form (syntax->list #'(head ...))])
          (syntax-parse form
            [dv:define-values-form
             #'([(dv.var ...) dv.rhs])]
            [_:define-syntaxes-form
             #'()]
            [e
             #'([() (begin e (values))])]))]
       [(clause ...) #'(preclause ... ...)])
    #'(letrec-syntaxes+values 
          ((svars srhs) ...)
        (clause ...)
        (begin tail))))

(convert-block
 (define x 1)
 (define y 2)
 (+ x y))

(define-syntax-class bindings
  (pattern ((var:id e) ...)
           #:with vars #'(var ...)))

(define-syntax-class sorted
  (pattern (n:nat ...)
           #:where (sorted? (syntax->datum #'(n ...)))))

(define (sorted? ns)
  (define (loop ns min)
    (cond [(pair? ns)
           (and (<= min (car ns))
                (loop (cdr ns) (car ns)))]
          [(null? ns) #t]))
  (loop ns -inf.0))

(define-syntax madd1
  (syntax-patterns
   [(_ e:expr/num)
    #'(+ 1 e)]))

(define-syntax mapp-to-1
  (syntax-patterns
   [(_ e)
    #:declare e expr/num->num
    #'(e 1)]))

(define-syntax bad-mapp-to-1
  (syntax-patterns
   [(_ e:expr/num->num)
    #'(e 'whoa)]))

#;
(define-syntax (madd2 stx)
  (syntax-parse stx
   [(_ e:expr/nat)
    #'(+ 2 e)]))


(define-syntax-class expr/nat
  (pattern e
           #:declare e (expr/c #'number?)))

(define-syntax-class cond-clauses
  (union
   (pattern ([#:else answer])
            #:with tests (list #'#t)
            #:with answers (list #'answer))
   (pattern ([test answer] . more:cond-clauses)
            #:with tests (cons #'test #'more.tests)
            #:with answers (cons #'answer #'more.answers))
   (pattern ([test #:=> answer] . more:cond-clauses)
            #:with tests (cons #'test #'more.tests)
            #:with answers (cons #'answer #'more.answers))
   (pattern ()
            #:with tests null
            #:with answers null)))


(define-syntax-class zork
  (pattern f:frob))
(define-syntax-class frob
  (pattern x:id))
