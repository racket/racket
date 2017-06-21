#lang scheme
(require syntax/parse
         syntax/parse/debug
         rackunit
         "setup.rkt")
(require (for-syntax syntax/parse))

;; Error message tests

(tcerr "define-conventions non id"
       (let () (define-conventions "foo") 0)
       #rx"^define-conventions: "
       #rx"expected name or name with formal parameters")

(tcerr "define-literal-set non id"
       (let () (define-literal-set "foo" (+ -)) 0)
       #rx"^define-literal-set: "
       #rx"expected identifier")

#|
(tcerr "parse-rhs: no variants"
       (let () (define-syntax-class x) 0)
       #rx"^define-syntax-class: "
       #rx"expected at least one variant")

(tcerr "parse-rhs (splicing): no variants"
       (let () (define-splicing-syntax-class x) 0)
       #rx"^define-splicing-syntax-class: "
       #rx"expected at least one variant")
|#

(tcerr "parse-rhs: commit and not delimit-cut"
       (let ()
         (define-syntax-class foo
           #:commit
           #:no-delimit-cut
           (pattern x))
         0)
       #rx"^define-syntax-class: "
       #rx"not allowed after")

(tcerr "parse-rhs: incompatible attrs flags"
       (let ()
         (define-syntax-class foo
           #:attributes (x)
           #:auto-nested-attributes
           (pattern x))
         0)
       #rx"^define-syntax-class: "
       #rx"not allowed after")

(tcerr "parse-variants: not a variant"
       (let () (define-syntax-class x y) 0)
       #rx"^define-syntax-class: "
       #rx"expected syntax-class variant")

;(tcerr "check-literals-bound: unbound literal"
;       (let () (define-syntax-class x #:literals (foo) (pattern (foo))) 0)
;       #rx"^define-syntax-class: "
;       #rx"unbound identifier not allowed as literal")

(tcerr "check-literals-bound: unbound literal"
       (let () (define-literal-set x (foo)) 0)
       #rx"^define-literal-set: "
       #rx"literal is unbound in phase 0")

(tcerr "check-literals-bound: unbound literal"
       (syntax-parse #'x #:literals (define defunky) [_ 'ok])
       #rx"^syntax-parse: "
       #rx"literal is unbound in phase 0")

(tcerr "append-lits+litsets: duplicate"
       (let () 
         (define-literal-set lits1 (+))
         (define-syntax-class x
           #:literals (+)
           #:literal-sets (lits1)
           (pattern foo))
         0)
       #rx"^define-syntax-class: "
       #rx"duplicate literal declaration")

(tcerr "parse-variant: too much"
       (let ()
         (define-syntax-class x
           (pattern y z)))
       #rx"^define-syntax-class: "
       #rx"unexpected terms")

(tcerr "parse-whole-pattern: declared not used"
       (syntax-parse #'1
         [x
          #:declare y nat
          'ok])
       #rx"^syntax-parse: "
       #rx"identifier in #:declare clause does not appear in pattern")

(tcerr "parse-single-pattern: reserved"
       (syntax-parse #'1
         [~and 'ok])
       #rx"^syntax-parse: "
       #rx"not allowed here")

(tcerr "parse-pat:id: splicing not allowed"
       (let ()
         (define-splicing-syntax-class foo (pattern (~seq a b c)))
         (syntax-parse #'1
           [x:foo 'ok]))
       #rx"^syntax-parse: "
       #rx"splicing syntax class not allowed here")

(tcerr "parse-cdr-patterns: not list"
       (syntax-parse #'1
         [(~and x . y) 'ok])
       #rx"^syntax-parse: "
       #rx"expected sequence of patterns")

(tcerr "parse-cdr-patterns: empty"
       (syntax-parse #'1
         [(~and) 'ok])
       #rx"^syntax-parse: "
       #rx"expected at least one pattern")

(tcerr "parse-some-pattern: no heads"
       (syntax-parse #'1
         [(~and (~seq 1 2)) 'ok])
       #rx"^syntax-parse: "
       #rx"head pattern not allowed here")

(tcerr "parse-pat:dots: alt, not list"
       (syntax-parser
        [((~alt . x) ...) 'ok])
       #rx"^syntax-parser: "
       #rx"expected sequence of patterns")

(tcerr "parse-pat:dots: alt, empty"
       (syntax-parser
        [((~alt) ...) 'ok])
       #rx"^syntax-parser: "
       #rx"expected at least one pattern")

(tcerr "parse-pat:dots: or, not list"
       (syntax-parser
        [((~or . x) ...) 'ok])
       #rx"^syntax-parser: "
       #rx"expected sequence of patterns")

(tcerr "parse-pat:dots: or, empty"
       (syntax-parser
        [((~or) ...) 'ok])
       #rx"^syntax-parser: "
       #rx"expected at least one pattern")

(tcerr "parse-pat:fail: bad"
       (syntax-parser
        [(~fail . x) 'ok])
       #rx"^syntax-parser: "
       #rx"bad ~fail pattern")

(tcerr "check-list-pattern"
       (syntax-parser
        [((~seq x . y)) 'ok])
       #rx"^syntax-parser: "
       #rx"expected proper list pattern")

(tcerr "parse-ehpat/bounds: min"
       (syntax-parser
        [((~between x 1.0 9) ...) 'ok])
       #rx"^syntax-parser: "
       #rx"expected exact nonnegative integer")

(tcerr "parse-ehpat/bounds: max"
       (syntax-parser
        [((~between x 1 "foo") ...) 'ok])
       #rx"^syntax-parser: "
       #rx"expected exact nonnegative integer")

(tcerr "parse-ehpat/bounds: min>max"
       (syntax-parser
        [((~between x 3 2) ...) 'ok])
       #rx"^syntax-parser: "
       #rx"minimum larger than maximum")

(tcerr "parse-pattern-sides: bad declare"
       (syntax-parser
        [x #:fail-unless #t #f #:declare x nat 'ok])
       #rx"^syntax-parser: "
       #rx"#:declare can only appear")

(tcerr "grab-decls: bad sc"
       (syntax-parser
        [x #:declare x 5 'ok])
       #rx"^syntax-parser: "
       #rx"expected syntax class")

;; checker procedures... bleh

(tcerr "check-attr-arity-list"
       (let () (define-syntax-class x #:attributes x (pattern x)) 'ok)
       #rx"^define-syntax-class: "
       #rx"expected list of attribute declarations")

(tcerr "check-attr-arity"
       (let () (define-syntax-class x #:attributes ("foo" 0) (pattern x)) 'ok)
       #rx"^define-syntax-class: "
       #rx"expected attribute name")
;; two more

