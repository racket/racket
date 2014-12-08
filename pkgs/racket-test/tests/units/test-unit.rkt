#lang racket/load

(require (for-syntax racket/private/unit-compiletime
                     racket/private/unit-syntax))
(require "test-harness.rkt"
         ;unit
         scheme/unit)

(define-syntax (lookup-sig-mac stx)
  (parameterize ((error-syntax stx))
    (syntax-case stx ()
      ((_ id)
       #`#'#,(let ((s (lookup-signature #'id)))
               (list (map syntax-local-introduce (signature-vars s))
                     (map (lambda (def)
                            (cons (map syntax-local-introduce (car def))
                                  (syntax-local-introduce (cdr def))))
                          (signature-val-defs s))
                     (map (lambda (def)
                          (cons (map syntax-local-introduce (car def))
                                (syntax-local-introduce (cdr def))))
                          (signature-stx-defs s))))))))

(define-signature x-sig (x))
(define-signature x-sig2 (x))
(define-signature y-sig (y))
(define-signature z-sig (z))

(define-signature yz-sig (y z))
(define-signature xy-sig (x y))
(define-signature empty-sig ())
(define-signature b-sig (b))

(define-signature empty-sub extends empty-sig ())

(define-signature x-sub extends x-sig (xx))
(define-signature y-sub extends y-sig (yy))
(define-signature x-sub2 extends x-sig (x2))

 

;; Keyword errors
(test-syntax-error "misuse of import"
  import)
(test-syntax-error "misuse of export"
  export)
(test-syntax-error "misuse of init-depend"
  init-depend)
(test-syntax-error "misuse of link"
  link)
(test-syntax-error "misuse of only"
  only)
(test-syntax-error "misuse of except"
  except)
(test-syntax-error "misuse of prefix"
  prefix)
(test-syntax-error "misuse of rename"
  rename)
(test-syntax-error "misuse of tag"
  tag)

;; define-signature-forms syntax errors
(test-syntax-error "define-signature-form: missing arguments"
  (define-signature-form))
(test-syntax-error "define-signature-form: missing arguments"
  (define-signature-form (a b)))
(test-syntax-error "define-signature-form: too many arguments"
  (define-signature-form (a b c) 1 2))
(test-syntax-error "define-signature-form: dot"
  (define-signature-form (a b) . c))
(test-syntax-error "define-signature-form: set!"
  (let ()
    (define-signature-form (a b) b)
    (set! a 1)))

(test-syntax-error "define-signature-form: bad params"
  (define-signature-form 1 2))
(test-syntax-error "define-signature-form: bad params"
  (define-signature-form a 2))
(test-syntax-error "define-signature-form: name not id"
  (define-signature-form (1 a) 1))
(test-syntax-error "define-signature-form: param not id"
  (define-signature-form (a 1) 1))
(test-syntax-error "define-signature-form: param dot"
  (define-signature-form (a . b) 1))


;; define-signature syntax-errors
(test-syntax-error "define-signature: missing name"
  (define-signature))
(test-syntax-error "define-signature: missing sig"
  (define-signature x))
(test-syntax-error "define-signature: too many args"
  (define-signature x (a b) 1))
(test-syntax-error "define-signature: bad name"
  (define-signature 1 (a b)))
(test-syntax-error "define-signature: bad name"
  (define-signature x extends 1 (a b)))
(test-syntax-error "define-signature: not a signature"
  (define-signature x extends y12 (a b)))
(test-syntax-error "define-signature: not a signature"
  (let () (define-signature x extends x (a b))))
(test-syntax-error "define-signature: bad name"
  (define-signature (a . b) (a b)))
(test-syntax-error "define-signature: dot"
  (define-signature b . (a b)))
(test-syntax-error "define-signature: dot"
  (define-signature b (a b) . 2))
(test-syntax-error "define-signature: set!"
  (let ()
    (define-signature a (a))
    (set! a 1)))
(test-syntax-error "define-signature: bad sig"
  (define-signature x y))
(test-syntax-error "define-signature: bad sig"
  (define-signature x (1)))
(test-syntax-error "define-signature: bad sig"
  (define-signature x (a . b)))
(test-syntax-error "define-signature: bad signature form"
  (define-signature x ((a))))
(test-syntax-error "define-signature: bad signature form"
  (define-signature x ((define-signature))))
(test-syntax-error "define-values: malformed (in define-signature)"
  (define-signature x ((define-values 1 2))))
(test-syntax-error "define-signature: bad form (does not return list)"
  (let ()
    (define-signature-form (a b) 1)
    (define-signature x ((a 1)))))
(test-syntax-error "define-signature: unknown form"
  (let ()
    (define-signature-form (a b) (list #'(c d)))
    (define-signature x ((a 1)))
    1))
(test-syntax-error "define-signature: duplicate name"
  (define-signature x (a a)))
(test-syntax-error "define-signature: duplicate values"
  (define-signature x (a (define-values (a) 1))))
(test-syntax-error "define-signature: duplicate values"
  (define-signature x (a (define-values (b b) 1))))
(test-syntax-error "define-signature: duplicate values"
  (define-signature x (a (define-values (b) 1) (define-syntaxes (b) 1))))
(test-syntax-error "define-signature: duplicate values"
  (let ()
    (define-signature test (y))
    (define-signature x extends test ((define-values (y) 1)))))

;; define-signature
(test stx-bound-id=? #'((a b) () ())
      (let ()
        (define-signature x (a b))
        (lookup-sig-mac x)))

(let ()
  (define s7 (void))
  (define h (void))
  (define-signature super (s1 (define-values (s2 s3) s4) (define-syntaxes (s5 s6) (values #'s7 #'s7))))
  (test stx-bound-id=? #'((s1 a b f) (((s2 s3) . _) ((c d) . e) ((i) . j)) (((_ _ _ _ _) . _) _ ((g) . #'h)))
        (let ()
          (define-signature x extends super (a b (define-values (c d) e) f
                                               (define-syntaxes (g) #'h)
                                               (define-values (i) j)))
          (lookup-sig-mac x))))
(let ()
  (define s7 (void))
  (define h (void))
  (define-signature super (s1 (define-values (s2 s3) s4) (define-syntaxes (s5 s6) (values #'s7 #'s7))))
  (let ((a 1) (g 2) (j 3) (s1 4) (s2 5))
    (test stx-bound-id=?
          #'((s1 a b f) (((s2 s3) . _) ((c d) . e) ((i) . j)) (((_ _ _ _ _) . _) _ ((g) . #'h)))
          (let ()
            (define-signature x extends super (a b (define-values (c d) e) f
                                                 (define-syntaxes (g) #'h)
                                                 (define-values (i) j)))
            (lookup-sig-mac x)))))
(let ()
  (define s7 (void))
  (define h (void))
  (define-signature super (s1 (define-values (s2 s3) s4) (define-syntaxes (s5 s6) (values #'s7 #'s7))))
  (test stx-bound-id=?
        #'((s1 a b f) (((s2 s3) . _) ((c d) . e) ((i) . j)) (((_ _ _ _ _) . _) _ ((g) . #'h)))
        (let ()
          (define-signature x extends super (a b (define-values (c d) e) f
                                               (define-syntaxes (g) #'h)
                                               (define-values (i) j)))
          (let ((a 1) (g 2) (j 3))
            (lookup-sig-mac x)))))
(test stx-bound-id=? #'(((#f . a) b f) (((c d) . e) ((i) . (#f . j))) ((((#f . g)) . #'h)))
      (let ((a 1) (g 2) (j 3))
        (define-signature x (a b (define-values (c d) e) f
                               (define-syntaxes (g) #'h)
                               (define-values (i) j)))
        (lookup-sig-mac x)))
(let ()
  (define-signature-form (x y)
                         (list (cdr (syntax-e y))))
  (test stx-bound-id=? #'((a)
                          ()
                          ())
        (let ()
          (define-signature z ((x . a)))
          (lookup-sig-mac z))))

;; unit syntax errors (without sub-signatures)
(test-syntax-error "unit: bad sig import"
  (unit (import 1) (export)))
(test-syntax-error "unit: bad sig export"
  (unit (import) (export 1)))
(test-syntax-error "unit: unknown sig import"
  (unit (import a) (export)))
(test-syntax-error "unit: unknown sig export"
  (unit (import) (export a)))
(test-syntax-error "unit: bad tag (not identifier)"
  (unit (import (tag 1 empty-sig)) (export)))
(test-syntax-error "unit: bad tag (not identifier)"
  (unit (import) (export (tag 'a empty-sig))))
(test-syntax-error "define-values: bad syntax (in unit)"
  (unit (import) (export) (define-values)))
(test-syntax-error "unit: multiple definition"
  (unit (import) (export) (define-values (x x) (values 1 2))))
(test-syntax-error "unit: multiple definition"
  (unit (import) (export) (define-syntaxes (x x) (values 1 2))))
(test-syntax-error "unit: multiple definition"
  (unit (import) (export) (define x 1) (define x 2)))
(test-syntax-error "unit: multiple definition"
  (unit (import) (export) (define-syntax x 1) (define-syntax x 2)))
(test-syntax-error "unit: multiple definition"
  (unit (import) (export) (define x 1) (define-syntax x 2)))
(test-syntax-error "unit: re-export"
  (unit (import x-sig) (export x-sig) (define x 1)))
(test-syntax-error "unit: redefine import"
  (unit (import x-sig) (export) (define x 1)))
(test-syntax-error "unit: set! import"
  (unit (import x-sig) (export) (set! x 1)))
(test-syntax-error "unit: set! export"
  (unit (import) (export x-sig) (define x 1) (set! x 1)))
(test-syntax-error "unit: undefined export"
  (unit (import) (export x-sig)))
(test-syntax-error "unit: undefined export"
  (unit (import) (export (prefix x: x-sig)) (define x 1)))
(test-syntax-error "unit: syntax export"
  (unit (import) (export x-sig) (define-syntax x 1)))
(test-syntax-error "unit: duplicate import"
  (unit (import x-sig x-sig2) (export)))
(test-syntax-error "unit: duplicate export"
  (unit (import) (export x-sig x-sig2) (define x 12)))
(test-syntax-error "unit: duplicate import signature"
  (unit (import x-sig (prefix a x-sig)) (export)))
(test-syntax-error "unit: duplicate export signature"
  (unit (import) (export x-sig (prefix a x-sig))
        (define x 1) (define ax 2)))
(test-syntax-error "unit: duplicate import signature"
  (unit (import (tag t x-sig) (tag t (prefix a x-sig))) (export)))
(test-syntax-error "unit: duplicate export signature"
  (unit (import) (export (tag t x-sig) (tag t (prefix a x-sig)))
        (define x 1) (define ax 2)))
(test-syntax-error "unit: duplicate export signature"
  (unit (import) (export x-sig x-sig)
        (define x 1)))


;; compound-unit syntax errors (without sub-signatures)
(test-syntax-error "compound-unit: bad import clause"
  (compound-unit (import (a empty-sig)) (export) (link)))
(test-syntax-error "compound-unit: import clause bad link id"
  (compound-unit (import (1 : empty-sig)) (export) (link)))
(test-syntax-error "compound-unit: import clause unknown sig"
  (compound-unit (import (a : empty-si)) (export) (link)))
(test-syntax-error "compound-unit: export bad link id"
  (compound-unit (import) (export a 1 b) (link)))
(test-syntax-error "compound-unit: link line bad link id"
  (compound-unit (import) (export) (link (((a : empty-sig)) b 1))))
(test-syntax-error "compound-unit: import clause bad sig id"
  (compound-unit (import (a : ())) (export) (link)))
(test-syntax-error "compound-unit: link line clause bad sig id"
  (compound-unit (import) (export) (link (((a : "")) b))))
(test-syntax-error "compound-unit: link line clause bad"
  (compound-unit (import) (export) (link (((a empty-sig)) b))))
(test-syntax-error "compound-unit: link line clause unknown"
  (compound-unit (import) (export) (link (((a : b)) b))))
(test-syntax-error "compound-unit: duplicate link ids"
  (compound-unit (import (x : x-sig) (x : y-sig)) (export) (link)))
(test-syntax-error "compound-unit: duplicate link ids"
  (compound-unit (import) (export) (link (((x : x-sig) (x : y-sig)) u))))
(test-syntax-error "compound-unit: duplicate link ids"
  (compound-unit (import (x : x-sig)) (export) (link (((x : x-sig)) u))))
(test-syntax-error "export: unbound link id"
  (compound-unit (import) (export a) (link)))
(test-syntax-error "link link: unbound link id"
  (compound-unit (import) (export) (link (() u a))))
(test-syntax-error "compound-unit: re-export"
  (compound-unit (import (S : x-sig)) (export S) (link)))
(test-syntax-error "compound-unit: re-export"
  (compound-unit (import (tag s (S : x-sig)))  (export (tag t S)) (link)))
(test-syntax-error "compound-unit: duplicate export signature"
  (compound-unit (import) (export X1 X2)
                 (link (((X1 : x-sig)) (unit (import) (export x-sig) (define x 1)))
                       (((X2 : x-sig)) (unit (import) (export x-sig) (define x 1))))))
(test-syntax-error "compound-unit: duplicate export signature"
  (compound-unit (import) (export (tag t X1) (tag t X2))
                 (link (((X1 : x-sig)) (unit (import) (export x-sig) (define x 1)))
                       (((X2 : x-sig)) (unit (import) (export x-sig) (define x 1))))))

;; define-values/invoke-unit syntax errors
(test-syntax-error "define-values/invoke-unit: no unit"
  (define-values/invoke-unit))
(test-syntax-error "define-values/invoke-unit: dot"
  (define-values/invoke-unit x y . x))
(test-syntax-error "define-values/invoke-unit: bad sig"
  (define-values/invoke-unit 1 1))
(test-syntax-error "define-values/invoke-unit: duplicate exports"
  (define-values/invoke-unit (unit (import) (export (prefix x: x-sig) x-sig2)
                               (define x 1)
                               (define x:x 2))
    x-sig x-sig2))


;; simple units, compound-units, and invoke-units (no subtypes, no tags, no prefix/rename/etc, no fancy signatures)
(test 12
  (invoke-unit (unit (import) (export) 12)))

(test 3
 (invoke-unit
  (compound-unit (import) (export)
                 (link (((X : x-sig) (Y : y-sig)) (unit (import empty-sig z-sig)
                                                        (export y-sig x-sig)
                                                        (define x 1)
                                                        (define y 2))
                                                  Z E)
                       (((Z : z-sig) (E : empty-sig)) (unit (import x-sig y-sig)
                                                            (export empty-sig z-sig)
                                                            (define z 3)
                                                            3) X Y)))))
                                                                   

;; Test compound export with signatures containing overlapping names
(test (list 10 11 12)
  (let ((un (compound-unit (import) (export S U)
              (link (((S : x-sig)) (unit (import) (export x-sig) (define x 10)))
                    (((U : xy-sig)) (unit (import) (export xy-sig) (define x 11) (define y 12)))))))
    (invoke-unit
     (compound-unit (import) (export)
                    (link (((S : x-sig) (U : xy-sig)) un)
                          (((B : b-sig)) (unit (import x-sig) (export b-sig) (define b x)) S)
                          (() (unit (import b-sig xy-sig) (export) (list b x y)) B U))))))

(define-signature even-sig (even))
(define-signature odd-sig (odd))

(define even-unit
  (unit (import odd-sig)
        (export even-sig)
    (define (even x)
      (or (= 0 x) (odd (sub1 x))))))

(define odd-unit
  (unit (import even-sig)
        (export odd-sig)
    (define (odd x)
      (and (> x 0) (even (sub1 x))))
    (define x (odd 11))
    x))

(define run-unit
  (compound-unit (import)
                 (export)
    (link (((EVEN : even-sig)) even-unit ODD)
          (((ODD : odd-sig)) odd-unit EVEN))))

(test #t (invoke-unit run-unit))

(define-signature is-3x-sig (is-3x))
(define-signature is-3x+1-sig (is-3x+1))
(define-signature is-3x+2-sig (is-3x+2))

(define is-3x-unit
  (unit (import is-3x+2-sig)
        (export is-3x-sig)
    (define (is-3x x)
      (or (= 0 x) (is-3x+2 (sub1 x))))))

(define is-3x+2-unit
  (unit (import is-3x+1-sig)
        (export is-3x+2-sig)
    (define (is-3x+2 x)
      (and (> x 0) (is-3x+1 (sub1 x))))))

(define is-3x+1-unit
  (unit (import is-3x-sig)
        (export is-3x+1-sig)
    (define (is-3x+1 x)
      (and (> x 0) (is-3x (sub1 x))))))

(define 3x-compound1
  (compound-unit (import (IS-3X : is-3x-sig))
                 (export IS-3X+1 IS-3X+2)
    (link (((IS-3X+1 : is-3x+1-sig)) is-3x+1-unit IS-3X)
          (((IS-3X+2 : is-3x+2-sig)) is-3x+2-unit IS-3X+1))))

(define 3x-compound2
  (compound-unit (import)
                 (export IS-3X)
    (link (((IS-3X : is-3x-sig)) is-3x-unit IS-3X+2)
          (((IS-3X+1 : is-3x+1-sig)
            (IS-3X+2 : is-3x+2-sig)) 3x-compound1 IS-3X))))

(define 3x-run-unit
  (unit (import is-3x-sig is-3x+1-sig is-3x+2-sig)
        (export)
    (list (is-3x 1)
          (is-3x 3)
          (is-3x+1 5)
          (is-3x+1 7)
          (is-3x+2 4)
          (is-3x+2 8))))

(define 3x-compound3
  (compound-unit (import)
                 (export IS-3X IS-3X+1 IS-3X+2)
    (link (((IS-3X : is-3x-sig)) 3x-compound2)
          (((IS-3X+1 : is-3x+1-sig)
            (IS-3X+2 : is-3x+2-sig)) 3x-compound1 IS-3X)
          (() 3x-run-unit IS-3X IS-3X+1 IS-3X+2))))

(test (list #f #t #f #t #f #t)
  (invoke-unit 3x-compound3))

(test (list #t #t #t)
  (let ()
    (define-values/invoke-unit 3x-compound3 (import) (export is-3x-sig is-3x+1-sig is-3x+2-sig))
    (list (is-3x+2 8)
          (is-3x+1 7)
          (is-3x 6))))
(test (list #t #t #t)
  (let ()
    (define-values/invoke-unit 3x-compound3 (import) (export (only is-3x-sig is-3x) (except is-3x+1-sig) (prefix x: is-3x+2-sig)))
    (list (x:is-3x+2 8)
          (is-3x+1 7)
          (is-3x 6))))
(test (list #t #t #t)
  (let ()
    (define-values/invoke-unit 3x-compound3 (import) (export is-3x-sig is-3x+1-sig (rename is-3x+2-sig (y is-3x+2))))
    (list (y 8)
          (is-3x+1 7)
          (is-3x 6))))

;; Tags
(let ()
  (define u
    (unit (import x-sig (tag t (prefix t: x-sig)) (tag u (prefix u: x-sig)))
          (export)
          (list x t:x u:x)))
  (define v
    (unit (import)
          (export x-sig (tag t (prefix t: x-sig)) (tag u (prefix u: x-sig)))
          (define x 1)
          (define t:x 2)
          (define u:x 3)))
  (test '(3 1 2)
    (invoke-unit
     (compound-unit (import) (export)
                    (link (((X1 : x-sig) (X2 : (tag u x-sig)) (X3 : (tag t x-sig))) v)
                          (() u (tag t X1) X2 (tag u X3))))))
  (test '(3 1 2)
    (invoke-unit
     (compound-unit (import) (export)
       (link (((L1 : (tag a x-sig)) (L2 : (tag b x-sig)) (L3 : (tag c x-sig)))
              (compound-unit (import) (export (tag a X1) (tag b X2) (tag c X3))
                (link (((X1 : x-sig) (X2 : (tag u x-sig)) (X3 : (tag t x-sig))) v))))
             (()
              (compound-unit (import (X1 : x-sig) (X2 : (tag u x-sig)) (X3 : (tag t x-sig))) (export)
                (link (() u (tag t X1) X2 (tag u X3))))
              L1 (tag u L2) (tag t L3)))))))

(let ()
  (define-values/invoke-unit (unit (import) (export (tag t x-sig)) (define x 1)) (import) (export (tag t x-sig)))
  (test 1 x))

;; simple runtime errors (no subtyping, no deps)
(test-runtime-error exn:fail:contract? "compound-unit: not a unit"
  (compound-unit (import) (export) (link (() 1))))
(test-runtime-error exn:fail:contract? "compound-unit: missing import"
  (compound-unit (import) (export)
                 (link (() (unit (import x-sig) (export))))))
(test-runtime-error exn:fail:contract? "compound-unit: missing import"
  (compound-unit (import (X : x-sig)) (export)
                 (link (() (unit (import x-sig) (export))
                           (tag u X)))))
(test-runtime-error exn:fail:contract? "compound-unit: missing import"
  (compound-unit (import (X : x-sig)) (export)
                 (link (() (unit (import (tag u x-sig)) (export))
                           X))))
(test-runtime-error exn:fail:contract? "compound-unit: missing export"
  (compound-unit (import) (export)
                 (link (((X : x-sig)) (unit (import) (export))))))
(test-runtime-error exn:fail:contract? "compound-unit: missing export"
  (compound-unit (import) (export)
                 (link (((X : (tag u x-sig))) (unit (import) (export x-sig) (define x 1))))))
(test-runtime-error exn:fail:contract? "compound-unit: missing export"
  (compound-unit (import) (export)
                 (link (((X : x-sig)) (unit (import (tag u x-sig)) (export))))))

(test-runtime-error exn:fail:contract? "invoke-unit: not a unit"
  (invoke-unit 1))
(test-runtime-error exn:fail:contract? "invoke-unit: unit has imports"
  (invoke-unit (unit (import x-sig) (export) x)))

(test-runtime-error exn:fail:contract? "define-values/invoke-unit: not a unit"
  (define-values/invoke-unit 1 (import) (export)))
(test-runtime-error exn:fail:contract? "define-values/invoke-unit: has imports"
  (define-values/invoke-unit (unit (import x-sig) (export) x) (import) (export)))
(test-runtime-error exn:fail:contract? "define-values/invoke-unit: signature mismatch"
  (define-values/invoke-unit (unit (import) (export)) (import) (export x-sig)))

;; unit creation w/o signatures (including macros and prefixes/renames).

;; free vars
(let ((y 1)
      (z 10))
  (define u (unit (import) (export yz-sig)
                  (define y 2)
                  (define z 3)))
  (define u1 (unit (import) (export)
                   y))
  (define u2 (unit (import (only yz-sig z)) (export)
                   y))
  (define u3 (unit (import (except yz-sig y)) (export)
                   y))
  (define u4 (unit (import (prefix s: yz-sig)) (export)
                   y))
  (define u5 (unit (import (rename yz-sig (r y))) (export)
                   y))
  (define u6 (unit (import yz-sig) (export)
                   y))
  (define (l x)
    (invoke-unit
     (compound-unit (import) (export)
                    (link (((YZ : yz-sig)) u)
                          (() x YZ)))))
  (test 1 (invoke-unit u1))
  (test 1 (l u2))
  (test 1 (l u3))
  (test 1 (l u4))
  (test 1 (l u5))
  (test 2 (l u6))
  (test-runtime-error
   exn:fail:contract:variable?
   "undefined"
   (let ()
     (define-values/invoke-unit (unit-from-context yz-sig) (import) (export yz-sig))
     y))
  (test 1
    (let ()
      (let ((u (unit-from-context yz-sig)))
        (define-values/invoke-unit u (import) (export (prefix x: yz-sig)))
        x:y)))
  ;; Exporting and prefix don't work right because the shadower doesn't see the shadowed
  ;; bindings, I think.
  #;(test 1
      (let ((x:y 12)
            (x:z 10))
        (let ((u (unit-from-context (prefix x: yz-sig))))
          (define-values/invoke-unit u yz-sig)
          y)))
  #;(test 1
    (let ((x:y 12)
          (x:z 10))
      (define-signature t (y z))
      (let ((u (unit-from-context (prefix x: t))))
        (define-values/invoke-unit u t)
        y)))
  (test 12
    (let ((x:y 12)
          (x:z 10))
      (define-values/invoke-unit (unit-from-context (rename yz-sig (x:y y) (x:z z)))
        (import) (export yz-sig))
      y))
  (test 12
    (let ((x:y 12)
          (x:z 10))
      (define-signature t (y z))
      (let ()
        (define-values/invoke-unit (unit-from-context (rename t (x:y y) (x:z z))) (import) (export t))
        y))))

;; Test that a define-values can define both internal and exported vars
(test '(1 2)
  (invoke-unit
   (compound-unit (import) (export)
                  (link (((T : yz-sig)) (unit (import x-sig) (export yz-sig)
                                              (define-values (y a) (values 1 2))
                                              (define-values (b z) (values y a)))
                                        S)
                        (((S : x-sig)) (unit (import yz-sig) (export x-sig) (define x 3) (list y z)) T)))))


;; Test that internal macros can define exports
(test 1
  (invoke-unit
   (unit (import) (export x-sig)
         (define-syntax (y stx)
           (syntax-case stx ()
             ((_ x) #'(define x 1))))
         (y x)
         x)))
  
(define-signature fact-sig (fact n))

;; Test renaming, self-recursion, only, and except
(test 24
  (invoke-unit
   (compound-unit (import) (export)
     (link (((F : fact-sig)) (unit (import (except (rename fact-sig (f-in fact)) n))
                                   (export (rename fact-sig (f-out fact)))
                               (define n 1)
                               (define (f-out x) (if (= 0 x)
                                                     1
                                                     (* x (f-in (sub1 x))))))
                             F)
           (() (unit (import (only fact-sig fact)) (export)
                 (define n 2)
                 (fact 4))
               F)))))


;; Test import prefix
(test 1
  (invoke-unit
   (compound-unit (import) (export)
     (link (((S : x-sig)) (unit (import) (export x-sig) (define x 1)))
           (() (unit (import (prefix s: x-sig)) (export) s:x) S)))))

(define-signature sx (x))
(define-signature sy (y))
(define-signature sz (z))

;; Test separate signatures with overlapping bindings, and export renaming and prefix
(test '(1 2 3)
  (invoke-unit
   (compound-unit (import) (export)
     (link (((S : x-sig) (T : yz-sig) (U : xy-sig)) (unit (import) (export (rename x-sig (s:x x))
                                                                           (rename yz-sig (t:y y) (t:z z))
                                                                           (prefix u: xy-sig))
                                                      (define x 1) (define y 2) (define z 3)
                                                      (define s:x x) (define t:y y) (define t:z z) (define u:x x) (define u:y y)))
           (((SX : sx)) (unit (import (prefix s: x-sig)) (export sx) (define x s:x)) S)
           (((SY : sy)) (unit (import (prefix u: xy-sig)) (export sy) (define y u:y)) U)
           (((SZ : sz)) (unit (import (prefix t: yz-sig)) (export sz) (define z t:z)) T)
           (() (unit (import sx sy sz) (export) (list x y z)) SX SY SZ)))))


;; Test units importing and exporting b, where lexical definition of b shadows
;; the b identifier in the signature
(test 2
  (let ((b 1))
    (define u1 (unit (import) (export b-sig) (define b 2)))
    (define u2 (unit (import b-sig) (export) b))
    (invoke-unit (compound-unit (import) (export)
                                (link (((B : b-sig)) u1)
                                      (() u2 B))))))
(test 1
  (let ((b 1))
    (define u1 (unit-from-context b-sig))
    (let ((b 2))
      (define-values/invoke-unit u1 (import) (export b-sig))
      b)))

(let ((x 1)
      (v 2))
  (let-syntax ((s (syntax-rules () ((_) (list x v)))))
    (define-signature t (x (define-syntaxes (s)
                                                (syntax-rules ()
                                                  ((_) (list x v))))
                           (define-values (v) (add1 x))))
    (define-signature t2 (x (define-syntaxes (s)
                                                (syntax-rules ()
                                                  ((_) (list x v))))
                           (define-values (v) (add1 x))))
    (define u3 (unit (import) (export t)
                     (define x 3)))
    (define u4 (unit (import) (export t2)
                     (define x 4)))
    (define (i u)
      (invoke-unit
       (compound-unit (import) (export)
                      (link (((T3 : t)) u3)
                            (((T4 : t2)) u4)
                            (() u T3 T4)))))
    ;; prefix
    (let ((x 5)
          (v 6))
      (let-syntax ((s (syntax-rules () ((_) (list x v)))))
        (test '(7 8 (7 8) 3 4 (3 4) 4 5 (4 5))
          (i (unit (import (prefix p: t) (prefix q: t2)) (export)
                   (define x 7)
                   (define v 8)
                   (define-syntax s (syntax-rules () ((_) (list x v))))
                   (list x v (s) p:x p:v (p:s) q:x q:v (q:s)))))
        (test '(5 6 (5 6) 3 4 (3 4) 4 5 (4 5))
          (i (unit (import (prefix p: t) (prefix q: t2)) (export)
                   (list x v (s) p:x p:v (p:s) q:x q:v (q:s)))))))
    (test '(7 8 (7 8) 3 4 (3 4) 4 5 (4 5))
      (i (unit (import (prefix p: t) (prefix q: t2)) (export)
               (define x 7)
               (define v 8)
               (define-syntax s (syntax-rules () ((_) (list x v))))
               (list x v (s) p:x p:v (p:s) q:x q:v (q:s)))))
    (test '(1 2 (1 2) 3 4 (3 4) 4 5 (4 5))
      (i (unit (import (prefix p: t) (prefix q: t2)) (export)
               (list x v (s) p:x p:v (p:s) q:x q:v (q:s)))))
    ;; only
    (let ((x 5)
          (v 6))
      (let-syntax ((s (syntax-rules () ((_) (list x v)))))
        (test '(7 8 (7 8) (3 4) (4 5))
          (i (unit (import (prefix p: (only t s)) (only (prefix q: t2) q:s)) (export)
                   (define x 7)
                   (define v 8)
                   (define-syntax s (syntax-rules () ((_) (list x v))))
                   (list x v (s) (p:s) (q:s)))))
        (test '(5 6 (5 6) (3 4) (4 5))
          (i (unit (import (prefix p: (only t s)) (prefix q: (only t2 s))) (export)
                   (list x v (s) (p:s) (q:s)))))))
    (test '(7 8 (7 8) (3 4) (4 5))
      (i (unit (import (only (prefix p: t) p:s) (only (prefix q: t2) q:s)) (export)
               (define x 7)
               (define v 8)
               (define-syntax s (syntax-rules () ((_) (list x v))))
               (list x v (s) (p:s) (q:s)))))
    (test '(1 2 (1 2) (3 4) (4 5))
      (i (unit (import (prefix p: (only t s)) (prefix q: (only t2 s))) (export)
               (list x v (s) (p:s) (q:s)))))
    ;;rename
    (let ((x 5)
          (v 6))
      (let-syntax ((s (syntax-rules () ((_) (list x v)))))
        (test '(7 8 (7 8) 3 4 (3 4) 4 5 (4 5))
          (i (unit (import (rename t (p:x x) (p:v v) (p:s s))
                           (rename t2 (q:x x) (q:v v) (q:s s)))
                   (export)
                   (define x 7)
                   (define v 8)
                   (define-syntax s (syntax-rules () ((_) (list x v))))
                   (list x v (s) p:x p:v (p:s) q:x q:v (q:s)))))
        (test '(5 6 (5 6) 3 4 (3 4) 4 5 (4 5))
          (i (unit (import (rename t (p:x x) (p:v v) (p:s s))
                           (rename t2 (q:x x) (q:v v) (q:s s)))
                   (export)
                   (list x v (s) p:x p:v (p:s) q:x q:v (q:s)))))))
    (test '(7 8 (7 8) 3 4 (3 4) 4 5 (4 5))
      (i (unit (import (rename t (p:x x) (p:v v) (p:s s))
                       (rename t2 (q:x x) (q:v v) (q:s s)))
               (export)
               (define x 7)
               (define v 8)
               (define-syntax s (syntax-rules () ((_) (list x v))))
               (list x v (s) p:x p:v (p:s) q:x q:v (q:s)))))
    (test '(1 2 (1 2) 3 4 (3 4) 4 5 (4 5))
      (i (unit (import (rename t (p:x x) (p:v v) (p:s s))
                       (rename t2 (q:x x) (q:v v) (q:s s)))
               (export)
               (list x v (s) p:x p:v (p:s) q:x q:v (q:s))))))
  )

(let ()
  (define-signature x ((define-syntaxes (m)
                         (syntax-rules ()
                           ((_ x) (define-syntax x
                                    (syntax-rules ()
                                      ((_ y) y))))))
                       (define-values (v)
                         (let ()
                           (m a)
                           (a 1)))))
  (test 1
    (invoke-unit
      (compound-unit (import) (export)
                     (link (((X : x)) (unit (import) (export x)))
                           (() (unit (import x) (export) v) X))))))
(let ()
  (define-signature x ((define-syntaxes (m)
                         (syntax-rules ()
                           ((_ x) (define-syntax x #'1))))
                       (define-syntaxes (m2)
                         (lambda (stx)
                           (syntax-case stx ()
                             ((_ x) (syntax-local-value #'x)))))
                       (define-values (v)
                         (let ()
                           (m a)
                           (m2 a)))))
  (test 1
    (invoke-unit
      (compound-unit (import) (export)
                     (link (((X : x)) (unit (import) (export x)))
                           (() (unit (import x) (export) v) X))))))

(let ()
  (define-signature x ((define-syntaxes (m) #'1)
                       (define-syntaxes (m2)
                         (lambda (stx)
                           (syntax-case stx ()
                             ((_ x) (syntax-local-value #'x)))))
                       (define-values (v)
                         (let ()
                           (m2 m)))))
  (test 1
    (invoke-unit
      (compound-unit (import) (export)
                     (link (((X : x)) (unit (import) (export x)))
                           (() (unit (import x) (export) v) X))))))


(let ()
  (define-signature s1 (a (define-values (x y) (values 1 2))))
  (define-signature s2 extends s1 ((define-values (z) (list a x))))
  (define u1 (unit (import s2) (export) (cons y z)))
  (define u2 (unit (import) (export s2) (define a 123)))
  (test (list 2 123 1) (invoke-unit (compound-unit (import) (export)
                                      (link (((a : s2)) u2)
                                            (() u1 a))))))
(let ()
  (define-signature s1 (a (define-values (x y) (values 1 2))))
  (let ((x 12))
    (define-signature s2 extends s1 ((define-values (z) (list a x))))
    (define u1 (unit (import s2) (export) (cons y z)))
    (define u2 (unit (import) (export s2) (define a 123)))
    (test (list 2 123 1) (invoke-unit (compound-unit (import) (export)
                                                     (link (((a : s2)) u2)
                                                           (() u1 a)))))))

(let ([c 5])
  (define-signature s1 (a (define-values (x y) (values c 2))))
  (define-signature s2 extends s1 (c (define-values (z) (list a x))))
  (define u1 (unit (import s2) (export) (cons y z)))
  (define u2 (unit (import) (export s2) (define a 123) (define c 43)))
  (test (list 2 123 5) (invoke-unit (compound-unit (import) (export)
                                                   (link (((a : s2)) u2)
                                                         (() u1 a))))))

;; Test define-syntaxes and define-values, without except, only, prefix and rename
;; Check the scoping
(let ((a 'abad)
      (b 'bbad)
      (c 'cbad)
      (v1 'v1bad)
      (v2 'v2bad)
      (s1 's1bad)
      (s2 's2bad)
      (strange-fact 'sfbad)
      (z 'zbad))
  (define z 1)
  (define a 'abad2)
  (define c 'cbad2)
  (define strange-fact 'sfbad4)
  (define-signature macro (a b c
                             (define-values (v1) (list a b c z 2))
                             (define-values (v2) (s2 a b c))
                             (define-values (strange-fact)
                                                (lambda (x)
                                                  (if (= x 0) (list z a b c) (cons x (strange-fact (sub1 x))))))
                             (define-syntaxes (s1 s2)
                                                  (values
                                                   (syntax-rules ()
                                                     ((_ a1 b1 c1) (list a b c v1 a1 b1 c1 z)))
                                                   (syntax-rules ()
                                                     ((_ a1 b1 c1) (s1 a1 b1 c1)))))))
  (let ((b 'bbad2)
        (c 'cbad3))
    (define z 3)
    (define u1
      (unit (import macro) (export)
        (define z 4)
        (list a b c v1 v2 (strange-fact 5) (s1 6 7 8) (s2 9 10 11))))
    (define u2
      (unit (import) (export macro)
         (define a 12)
         (define b 13)
         (define c 14)))
    (test '(12 13 14
               (12 13 14 1 2)
               (12 13 14 (12 13 14 1 2) 12 13 14 1)
               (5 4 3 2 1 1 12 13 14)
               (12 13 14 (12 13 14 1 2) 6 7 8 1)
               (12 13 14 (12 13 14 1 2) 9 10 11 1))
      (invoke-unit
       (compound-unit (import) (export)
         (link (((U2 : macro)) u2)
               (() u1 U2)))))))
  

;; We can re-define imported values
(let ()
  (define-signature s ((define-values (y) 1)))
  (define-signature t (z))
    (test 3
      (invoke-unit
       (compound-unit (import) (export)
                      (link (((T : t)) (unit (import s) (export t) (define y 3) (define z y)) S)
                            (((S : s)) (unit (import) (export s) (define y 1)))
                            (() (unit (import t) (export) z) T))))))

;; Can't use imports as pattern variables
#;(let ()
  (define-signature s (y (define-syntaxes (m) (syntax-rules (y) ((_ y) 1)))))
  (unit (import s) (export)
        (m y)))


(test '(2 3)
  (let ()
    (define-signature sig (y (define-values (v) (add1 y))))
    (let ()
      (define-values/invoke-unit
       (unit (import) (export sig) (define y 2))
       (import)
       (export sig))
      (list y v))))


;; I'm not sure that this should work.
#;(test '(2 3)
  (let ()
    (define-signature sig (y (define-values (v) (add1 y))))
    (define-values/invoke-unit
     (unit (import) (export sig) (define y 2))
     sig)
    (list y v)))
  
  

;; subtyping

(let ()
  (define u1 (unit (import x-sig) (export y-sub) (define y (add1 x)) (define yy 2) (list x y yy)))
  (define u2 (unit (import y-sig) (export x-sub) (define x 3) (define xx 44)))
  (define u3 (compound-unit (import (S1 : x-sig)) (export S4)
                            (link (((S4 : y-sub)) u1 S1))))
  (define u4 (compound-unit (import (S3 : y-sig)) (export S2)
                            (link (((S2 : x-sub)) u2 S3))))
  (define u5 (compound-unit (import (S2 : y-sub)) (export S1)
                            (link (((S1 : x-sig)) u2 S2))))
  (test '(3 4 2)
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((S1 : x-sig)) u2 S2)
                              (((S2 : y-sig)) u1 S1)))))
  (test '(3 4 2)
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((S1 : x-sub)) u2 S2)
                              (((S2 : y-sub)) u1 S1)))))
  
  
  (test '(3 4 2)
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((S1 : x-sub)) u4 S4)
                              (((S4 : y-sub)) u3 S1)))))
  (test '(3 4 2)
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((S1 : x-sig)) u4 S4)
                              (((S4 : y-sig)) u3 S1)))))
  
  
  (test '(3 4 2)
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((S1 : x-sig)) u5 S2)
                              (((S2 : y-sub)) u1 S1))))))
(let ()  
  (define u1 (unit (import) (export x-sig) (define x 1)))
  (define u2 (unit (import x-sub) (export)))
  
  (test-runtime-error exn:fail:contract? "compound-unit: not a subtype"
                      (compound-unit (import) (export)
                                     (link (((S : x-sub)) u1))))
  
  (test-runtime-error exn:fail:contract? "compound-unit: not a subtype"
                      (compound-unit (import) (export)
                                     (link (((S : x-sig)) u1)
                                           (() u2 S))))
  
  (test-runtime-error exn:fail:contract? "compound-unit: not a subtype"
                      (compound-unit (import (S : x-sig)) (export)
                                     (link (() u2 S)))))

(let ()
  (define u1 (unit (import) (export x-sub y-sub) (define x 1) (define xx 2) (define y 3) (define yy 4)))
  (define-values/invoke-unit u1 (import) (export x-sig))
  (test 1 x)
  (test-runtime-error exn? "unbound identifier" xx)
  (test-runtime-error exn? "unbound identifier" y)
  (test-runtime-error exn? "unbound identifier" yy))

(let ()
  (define u1 (unit (import) (export x-sig) (define x 1)))
  (test-runtime-error exn:fail:contract? "define-values/invoke-unit: not a subtype"
                      (define-values/invoke-unit u1 (import) (export x-sub))))

;; export-subtyping
(test-syntax-error "duplicate exports (subtypes)"
                   (unit (import) (export x-sig x-sub)
                         (define x 1)
                         (define xx 1)))
(test-syntax-error "duplicate exports (subtypes)"
                   (unit (import) (export x-sub x-sig)
                         (define x 1)
                         (define xx 1)))
(let ()
  (define u (unit (import) (export x-sub) (define x 1) (define xx 1)))
  (test-syntax-error "duplicate exports (subtypes)"
                     (compound-unit (import) (export l1 l2)
                                    (link (((l1 : s1)) u)
                                          (((l2 : s2)) u)))))
(let ()
  (define u (unit (import) (export x-sub (prefix x: x-sub2))
                  (define x 1)
                  (define xx 2)
                  (define x:x 3)
                  (define x:x2 4)))
  (define u2 (unit (import x-sig) (export)))
  (define v (unit (import x-sub) (export)
                  (+ x xx)))
  (define w (unit (import x-sub2) (export)
                  (+ x x2)))
  (define u3 (unit (import x-sub (prefix m: x-sub2)) (export)
                   (+ x xx m:x m:x2)))
  (test 3
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((S2 : x-sub)) u)
                              (() v S2)))))
  (test 7
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((S3 : x-sub2)) u)
                              (() w S3)))))
  (test 10
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((S3 : x-sub2) (S2 : x-sub)) u)
                              (() u3 S3 S2)))))
  (test-runtime-error exn:fail:contract? "ambiguous export"
                      (compound-unit (import) (export)
                                     (link (((S1 : x-sig)) u)))))
  (test-runtime-error exn:fail:contract? "ambiguous import"
                      (compound-unit (import (S1 : x-sub) (S2 : x-sub2)) (export)
                                     (link (() u2 S1 S2))))

(test-syntax-error "duplicate links (subtype)"
                    (compound-unit (import) (export)
                                     (link (((S1 : x-sig)) u3)
                                           (() u1 S2 S1)
                                           (((S2 : x-sig)) u3))))

;; tags
(let ()
  (define-signature s1 (a))
  (define-signature s2 extends s1 (b))
  (define-signature s3 extends s2 ())
  (define-signature s4 extends s3 ())
  (define u1
    (unit (import (prefix s1: s1) 
                  (tag t (prefix s2: s2))
                  (prefix bs1: s2) 
                  (prefix bs2: s3))
          (export)
          (list s1:a s2:a s2:b bs1:a bs2:b)))
  (define u2
    (unit (import) (export s3)
          (define a 1) (define b 2)))
  (define u3
    (unit (import) (export s2)
          (define a 3) (define b 4)))
  (test '(1 3 4 1 2)
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((S2a : s3)) u2)
                              (((S2b : s2)) u3)
                              (() u1 S2a (tag t S2b))))))
  (test-runtime-error exn:fail:contract? "compound-unit: signature mismatch"
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((S1 : s1)) u2)
                              (((S2 : s2)) u3)
                              (() u1 (tag t S1) S2))))))
(let ()
  (define u1
    (unit (import) (export (prefix a: x-sig) (tag t (prefix c: x-sig)))
          (define a:x 1)
          (define c:x 4)))
  (define u2
    (unit (import x-sig) (export)
          x))
  (define u3
    (unit (import x-sub) (export)
          (list x xx)))

  (test 4
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((S1 : (tag t x-sig)) (S2 : x-sig)) u1)
                              (() u2 S1)))))
  (test-runtime-error exn:fail:contract? "compound-unit: signature mismatch"
                      (invoke-unit
                       (compound-unit (import) (export)
                                      (link (((S1 : (tag t x-sub)) (S2 : x-sub)) u1)
                                            (() u2 S1)))))
  )

(let ()
  (define u1 (unit (import) (export (tag t1 x-sig) (prefix : x-sig))
                   (define x 10)
                   (define :x 11)))
  (define-values/invoke-unit u1 (import) (export x-sig (tag t1 (prefix m x-sig))))
  (test '(11 10)
        (list x mx)))


(define-signature s1 (x))
(define-signature s2 (a x z))


(test-syntax-error "unit-from-context: no sigs"
  (unit-from-context))
(test-syntax-error "unit-from-context: too many sigs"
  (unit-from-context s1 s2))
(test-syntax-error "unit-from-context: too many sigs"
  (unit-from-context s1 . s2))
(test-syntax-error "unit-from-context: bad sig"
  (unit-from-context 1))

(test-syntax-error "unit-from-context: no name"
  (define-unit-from-context))
(test-syntax-error "unit-from-context: no sigs"
  (define-unit-from-context s1))
(test-syntax-error "unit-from-context: no sigs"
  (define-unit-from-context n))
(test-syntax-error "unit-from-context: too many sigs"
  (define-unit-from-context n s1 s2))
(test-syntax-error "unit-from-context: too many sigs"
  (define-unit-from-context n s1 . s2))
(test-syntax-error "unit-from-context: bad sig"
  (define-unit-from-context n 1))



;; Test the struct form
(test-syntax-error "struct: missing name and fields"
  (define-signature x ((struct))))
(test-syntax-error "struct: missing name"
  (define-signature x ((struct n))))
(test-syntax-error "struct: bad name"
  (define-signature x ((struct 1 ()))))
(test-syntax-error "struct: bad fields (dot)"
  (define-signature x ((struct n (x . y)))))
(test-syntax-error "struct: bad fields"
  (define-signature x ((struct n 1))))
(test-syntax-error "struct: bad omission"
  (define-signature x ((struct n () t))))
(test-syntax-error "struct: bad omission (dot)"
  (define-signature x ((struct n () . -selectors))))
(test-syntax-error "struct: bad omission"
  (define-signature x ((struct n () x))))

(let ()
  (define-signature sig ((struct s (x y))))
  (test 3
    (invoke-unit
     (compound-unit (import) (export)
                    (link (((S : sig)) (unit (import) (export sig)
                                             (define-struct s (x y))))
                          (() (unit (import sig) (export)
                                    (match (make-s 1 2)
                                      ((struct s (a b)) (+ a b))))
                              S)))))
  (let ()
    (define-values/invoke-unit (unit (import) (export sig) (define-struct s (x y)))
      (import)
      (export sig))
    (test 3
      (match (make-s 1 2)
        ((struct s (a b)) (+ a b)))))
  (let ()
    (define u
      (unit (import) (export (rename sig (make-s/defaults make-s)))
            (define-struct s (x y))
            (define (make-s/defaults x)
              (make-s x 'default))))
    (define-values/invoke-unit u (import) (export sig))
    (test #t (s? (make-s 1))))

  (let ((set-s-x! 1))
    (define-signature sig ((struct s (x y))))
    (test 1
      (invoke-unit
       (compound-unit (import) (export)
                      (link (((S : sig)) (unit (import) (export sig) (define-struct s (x y))))
                            (() (unit (import sig) (export)
                                      set-s-x!) S))))))
  (let ((make-s 1))
    (define-signature sig ((struct s (x y) #:omit-constructor)))
    (test 1
      (invoke-unit
       (compound-unit (import) (export)
                      (link (((S : sig)) (unit (import) (export sig) (define-struct s (x y))))
                            (() (unit (import sig) (export)
                                      make-s) S)))))))

;; Dependencies

(define-signature s1 (a))
(define-signature s2 extends s1 ())

(define u1 (unit (import s1) (export) (init-depend s1)
                 a))
(define u2 (unit (import) (export s1)
                 (define a 12)))
(define u3 (unit (import (tag t s1)) (export) (init-depend (tag t s1))
                 a))

(define u4 (compound-unit (import (L : s2)) (export)
                          (link (() u1 L))))
(define u5 (unit (import) (export s2)
                 (define a 12)))
(test-syntax-error "unit: bad dependency"
                   (unit (import (tag t s1)) (export) (init-depend s1)))
(test-syntax-error "unit: bad dependency"
                   (unit (import s1) (export) (init-depend (tag t s1))))

(test 12 (invoke-unit (compound-unit (import) (export)
                                     (link (((S1 : s1)) u2)
                                           (() u1 S1)))))

(test-runtime-error exn:fail:contract? "Dependency violation"
                    (compound-unit (import) (export)
                                     (link (() u1 S1)
                                           (((S1 : s1)) u2))))

(test-runtime-error exn:fail:contract? "Dependency violation"
                    (compound-unit (import) (export)
                                     (link (() u3 (tag t S1))
                                           (((S1 : s1)) u2))))



(test-runtime-error exn:fail:contract? "Dependency violation"
                    (compound-unit (import) (export)
                                     (link (() u4 S2)
                                           (((S2 : s2)) u5))))
;; Inference


(define u (unit (import x-sig) (export y-sig) (define y 0) x))
(define v (unit (import) (export x-sig y-sig) (define x 9) (define y 10)))

(test 9
      (let ()
        (define-unit-binding u2 u (import x-sig y-sig) (export))
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((A : x-sig) (B : y-sig)) v)
                              (() u A B))))))

(test-runtime-error exn:fail:contract? "not subunit"
                    (let () (define-unit-binding u2 u (import x-sig) (export x-sig)) 1))
(test-runtime-error exn:fail:contract? "not subunit"
                    (let () (define-unit-binding u2 u (import) (export)) 1))
(test-runtime-error exn:fail:contract? "not a unit"
                    (let () (define-unit-binding u2 1 (import) (export)) 1))

(test-syntax-error "define-unit-binding: duplicate import"
                   (define-unit-binding u 1 (import x-sig x-sig) (export)))
(test-syntax-error "define-unit-binding: export subtypes"
                   (define-unit-binding u 1 (import) (export x-sig x-sub)))
(test-syntax-error "define-unit-binding: export subtypes"
                   (define-unit-binding u 1 (import) (export x-sub x-sig)))
(test-syntax-error "define-unit-binding: bad dependency"
                   (define-unit-binding u 1 (import x-sig) (export) (init-depend x-sub)))
(test-syntax-error "define-unit-binding: bad dependency"
                   (define-unit-binding u 1 (import x-sub) (export) (init-depend x-sig)))


(test-syntax-error "define-unit: missing name, import, export"
  (define-unit))
(test-syntax-error "define-unit: missing import, export"
  (define-unit a))
(test-syntax-error "define-unit: missing export"
  (define-unit a (import)))
(test-syntax-error "define-unit: missing name"
  (define-unit (import) (export)))
(test-syntax-error "define-unit: bad name"
  (define-unit "x" (import) (export)))
(test-syntax-error "define-unit: bad syntax"
  (define-unit x (unit (import) (export))))
(test-runtime-error exn:fail:contract? "define-unit: bad set!"
  (let ()
    (define-signature s ())
    (define-unit x (import) (export) 1)
    (set! x (unit (import s) (export) 1))))
(test-runtime-error exn:fail:contract? "define-unit: bad set!"
  (let ()
    (define-signature s ())
    (define-unit x (import) (export s) 1)
    (set! x (unit (import) (export) 1))))


(test-syntax-error "define-compound-unit: missing import"
  (define-compound-unit x))
(test-syntax-error "define-compound-unit: missing name"
  (define-compound-unit))
(test-syntax-error "define-compound-unit: missing name"
  (define-compound-unit (import) (link) (export)))
(test-syntax-error "define-compound-unit: bad name"
  (define-compound-unit 1 (import) (link) (export)))

(test-syntax-error "invoke-unit/infer : no unit"
  (invoke-unit/infer))
(test-syntax-error "invoke-unit/infer : not a unit"
  (invoke-unit/infer 1))
(test-syntax-error "invoke-unit/infer : not a unit"
  (let ([x 1]) (invoke-unit/infer x)))
(test-syntax-error "invoke-unit/infer : not a unit"
  (let-syntax ([x 1]) (invoke-unit/infer x)))
(test-syntax-error "invoke-unit/infer: too much"
  (invoke-unit/infer x y))

(define-unit u (import x-sig) (export))
(define-unit v (import) (export x-sig) (define x 3))

(test-syntax-error "invoke-unit/infer : no unit"
  (invoke-unit/infer (link)))
(test-syntax-error "invoke-unit/infer : not a unit"
  (invoke-unit/infer (link 1 u)))
(test-syntax-error "invoke-unit/infer : not a unit"
  (let ([x 1]) (invoke-unit/infer (link u x))))
(test-syntax-error "invoke-unit/infer : not a unit"
  (let-syntax ([x 1]) (invoke-unit/infer (link x u))))
(invoke-unit/infer (link u v))

(test-syntax-error "define-values/invoke-unit/infer: no unit"
  (define-values/invoke-unit/infer))
(test-syntax-error "define-values/invoke-unit/infer: not a unit"
  (define-values/invoke-unit/infer 1))
(test-syntax-error "define-values/invoke-unit/infer: not a unit"
  (let ((x 1))
    (define-values/invoke-unit/infer x)))
(test-syntax-error "define-values/invoke-unit/infer: not a unit"
  (let-syntax ((x 1))
    (define-values/invoke-unit/infer x)))
(test-syntax-error "define-values/invoke-unit/infer: too much"
  (define-values/invoke-unit/infer x y))

(define-unit u (import x-sig) (export) x)
(define-unit v (import) (export x-sig) (define x 3))

(test-syntax-error "define-values/invoke-unit/infer: no unit"
  (define-values/invoke-unit/infer (link)))
(test-syntax-error "define-values/invoke-unit/infer: not a unit"
  (define-values/invoke-unit/infer (link 1 u)))
(test-syntax-error "define-values/invoke-unit/infer: not a unit"
  (let ([x 1])
    (define-values/invoke-unit/infer (link u x))))
(test-syntax-error "define-values/invoke-unit/infer: not a unit"
  (let-syntax ([x 1])
    (define-values/invoke-unit/infer (link u x))))

(test-runtime-error
 exn:fail:contract:variable?
 "undefined"
 (let ()
   (define-values/invoke-unit/infer (link u v))
   x))

(test-runtime-error
 exn:fail:contract:variable?
 "undefined"
 (let ()
   (define-values/invoke-unit/infer (export x-sig) (link u v))
   x))

(let ()
  (define-values/invoke-unit/infer (export x-sig) v)
  x)
(test-syntax-error "define-values/invoke-unit/infer: doesn't export y"
  (define-values/invoke-unit/infer (export y-sig) (link u v)))

(test-runtime-error exn? "define-values/invoke-unit/infer: unbound variable: x"
  (let ()
    (define-values/invoke-unit/infer (export) (link u v))
    x))
(test-syntax-error "define-values/invoke-unit/infer: doesn't export y"
  (define-values/invoke-unit/infer (export y-sig) v))
(test-runtime-error exn? "define-values/invoke-unit/infer: unbound variable: x"
   (let ()
     (define-values/invoke-unit/infer (export) v)
     x))

(let ()
  (define-signature s^ (a))
  (define-signature t^ (b))
  (define-unit u@
    (import s^)
    (export t^)
    (init-depend s^)
    (define b a))
  (define-unit v@
    (import)
    (export s^)
    (define a 2))
  (define-values/invoke-unit/infer (export) (link v@ u@))
  (test-runtime-error exn? "define-values/invoke-unit/infer: init-depend broken"
    (define-values/invoke-unit/infer (export) (link u@ v@))))

(define-unit u (import x-sig) (export) x)
(test-syntax-error "define-values/invoke-unit/infer: bad imports"
  (define-values/invoke-unit/infer u))
(define-unit u (import x-sig y-sig) (export))
(test-syntax-error "define-values/invoke-unit/infer: bad imports"
  (define-values/invoke-unit/infer u))
(define-unit u (import) (export x-sig y-sig)
  (define x 10)
  (define y 20))
(test 30
  (let ()
    (define-values/invoke-unit/infer u)
    (+ y x)))

(test 1
  (let ()
    (define-unit x (import) (export) 1)
    (invoke-unit x)))
(test 1
  (let ()
    (define-unit x (import) (export) 1)
    (let ((u 1))
      (invoke-unit x))))
(test 2
  (let ()
    (define-unit x (import) (export) 1)
    (set! x (unit (import) (export) 2))
    (invoke-unit x)))



(let ()
  (define-signature s1 (a))
  (define-signature s2 extends s1 (b))
  (define-signature s3 (c))
  (define-signature s4 extends s3 (d))
  (define-unit u (import s2) (export s3) (define c (+ a b)))
  (define-unit v (import) (export s2) (define a 1) (define b 3))
  (set! u (unit (import s1) (export s4) (define c (add1 a)) (define d 12)))
  (let ()
    (define-values/invoke-unit (compound-unit/infer (import) (export s2 s3) (link v u)) 
      (import) (export s2 s3))
    (test '(1 3 2) (list a b c))))


(test-syntax-error "compound-unit/infer: missing export"
  (compound-unit/infer (link) (import)))
(test-syntax-error "compound-unit/infer: bad unit"
  (compound-unit/infer (import) (export) (link 1)))
(test-syntax-error "compound-unit/infer: bad import"
  (compound-unit/infer (import (a : b)) (export) (link)))
(test-syntax-error "compound-unit/infer: bad link"
  (compound-unit/infer (import) (export) (link (((A : b)) c))))
(test-syntax-error "compound-unit/infer: unknown sig"
  (compound-unit/infer (import ??) (export) (link)))
(test-syntax-error "compound-unit/infer: unknown sig"
  (compound-unit/infer (import) (export ??) (link)))
(test-syntax-error "compound-unit/infer: unknown sig"
  (compound-unit/infer (import) (export) (link (() u ??))))


(define-unit x
  (import x-sig)
  (export y-sig)
  (define y x)
  y)

(define-unit y
  (import y-sig)
  (export (rename x-sig (x x)))
  (define x y)
  x)

(define-unit z
  (import (prefix : x-sig) y-sig)
  (export)
  (+ :x y))

(define-unit a
  (import)
  (export x-sig y-sig z-sig)
  (define x 1)
  (define y 2)
  (define z 3))

(define-unit b
  (import x-sig y-sig z-sig)
  (export)
  (+ x y z))

(test-syntax-error "compound-unit/infer: re-export"
  (compound-unit/infer (import (l : x-sig)) (export x-sig) (link)))
(test-syntax-error "compound-unit/infer: duplicate def and import"
  (compound-unit/infer (import y-sig x-sig) (export) (link x y)))
(test-syntax-error "compound-unit/infer: unprovided sig"
  (compound-unit/infer (import) (export) (link x)))
(test-syntax-error "compound-unit/infer: unprovided sig"
  (compound-unit/infer (import) (export x-sig) (link)))

(test-runtime-error
 exn:fail:contract:variable?
 "undefined"
 (invoke-unit 
  (compound-unit/infer (import) (export)
                       (link x y))))

(test 3
  (let ()
    (define-signature s (x y))
    (let ((x 1)
          (y 2))
      (define-unit-from-context u1 s)
      (define-unit u2 (import (prefix : s)) (export)
        (+ :x :y))
      (invoke-unit
       (compound-unit/infer (import) (export)
                            (link u1 u2))))))
(test 6
      (invoke-unit
       (compound-unit/infer (import) (export)
                            (link (((L1 : y-sig)) a)
                                  x
                                  (() b L1)))))

(let ()
  (define-unit u1 (import (tag t x-sig)) (export)
    (add1 x))
  (define-unit u2 (import) (export x-sig)
    (define x 2))
  (test 3
        (invoke-unit
         (compound-unit/infer (import) (export)
                              (link u2 u1)))))
(let ()
  (define-unit u1 (import x-sig) (export)
    (add1 x))
  (define-unit u2 (import) (export (tag u x-sig))
    (define x 2))
  (test 3
        (invoke-unit
         (compound-unit/infer (import) (export)
                              (link u2 u1)))))

(let ()
  (define-unit x2 (import) (export x-sig) (define x 44))
  (define-unit x3 (import) (export x-sig) (define x 4400))
  (define-unit z (import x-sig) (export z-sig) (define z (+ 100 x)))
  (define-compound-unit/infer u (import (L : x-sig)) (export L2 y-sig)
    (link x3 (((L2 : z-sig)) z L) (() a L)))
  (test 190
        (invoke-unit
         (compound-unit/infer (import) (export)
                              (link x2 u b)))))

#;
(let ()
  (define-unit u (import x-sig) (export))
  (define-unit v (import) (export x-sub) (define x 12) (define xx 13))
  (define-compound-unit/infer c (import) (export x-sig) (link v u))
  (define-values/invoke-unit/infer c)
  (test 12 x))


(let ()
  (define-unit u (import) (export x-sig)
    (define x 12))
  (define-unit u2 (import) (export x-sig)
    (define x 13))
  (define-unit v (import) (export y-sig)
    (define y 11))
  (define-unit v2 (import) (export y-sig)
    (define y 1))
  (define-unit u3 (import y-sig x-sig) (export)
    (+ y x))
  (test 24
        (invoke-unit
         (compound-unit/infer (import) (export)
                              (link (((l : x-sig)) u)
                                    (((l2 : x-sig)) u2)
                                    (((l3 : y-sig)) v)
                                    (((l4 : y-sig)) v2)
                                    (() u3 l2 l3))))))

;; unit/new-import-export

(test-runtime-error exn:fail:contract? "unit/new-import-export: not a unit"
  (unit/new-import-export (import) (export)
                          (() 1)))

(test-runtime-error exn:fail:contract? "unit/new-import-export: not a subtype"
  (unit/new-import-export (import) (export)
                          ((x-sig) (unit (import) (export)))))


(test-runtime-error exn:fail:contract? "unit/new-import-export: not a subtype"
  (unit/new-import-export (import) (export)
                          (() (unit (import x-sig) (export)))))

(define-unit u (import x-sig) (export y-sig)
  (define y x))

(test-syntax-error "unit/new-import-export: not enough imports"
  (unit/new-import-export (import) (export x-sig)
                          ((y-sig) u x-sig)))

(test-syntax-error "unit/new-import-export: too many exports"
  (unit/new-import-export (import x-sig) (export y-sig z-sig)
                          ((y-sig) u x-sig)))

(let ()
  (define-unit u 
    (import xy-sig) (export z-sig)
    (define z (+ x y)))
  (define-unit v
    (import) (export x-sig y-sig)
    (define x 4)
    (define y 8))
  (define-unit w (import z-sig) (export)
    z)
  (define-unit/new-import-export u2 (import x-sig y-sig) (export z-sig)
    ((z-sig) u xy-sig))
  (test 12
        (invoke-unit (compound-unit/infer (import) (export)
                                          (link v u2 w)))))
      
(let ()
  (define-unit u 
    (import x-sig y-sig) (export z-sig)
    (define z (+ x y)))
  (define-unit v
    (import) (export xy-sig)
    (define x 4)
    (define y 8))
  (define-unit w (import z-sig) (export)
    z)
  (define-unit/new-import-export u2 (import xy-sig) (export z-sig)
    ((z-sig) u y-sig x-sig))
  (test 12
        (invoke-unit (compound-unit/infer (import) (export)
                                          (link v u2 w)))))
      
(let ()
  (define-unit u 
    (import xy-sig) (export z-sig)
    (define z (+ x y)))
  (define-unit v
    (import) (export x-sig y-sig)
    (define x 4)
    (define y 8))
  (define-unit w (import z-sig) (export)
    z)
  (define-unit/new-import-export v2 (import) (export xy-sig)
    ((x-sig y-sig) v))
  (test 12
        (invoke-unit (compound-unit/infer (import) (export)
                                          (link v2 u w)))))
      
(let ()
  (define-unit u 
    (import x-sig y-sig) (export z-sig)
    (define z (+ x y)))
  (define-unit v
    (import) (export xy-sig)
    (define x 4)
    (define y 8))
  (define-unit w (import z-sig) (export)
    z)
  (define-unit/new-import-export v2 (import) (export y-sig x-sig)
    ((xy-sig) v))
  (test 12
        (invoke-unit (compound-unit/infer (import) (export)
                                          (link v2 u w)))))




;; open
(let ()
  (define-signature xzy
    ((open x-sig) (open y-sig) (open z-sig)))
  
  (define-unit u (import xzy) (export)
    (+ x z y))
  
  (define-unit v (import) (export xzy)
    (define x 10)
    (define y 20)
    (define z 30))
  
  (test 60
        (invoke-unit (compound-unit/infer (import) (export) (link v u)))))

(let ([x 1]
      [y 2]
      [z 3])  
  (define-signature xzy
    ((open x-sig) (open y-sig) (open z-sig)))
  
  (define-unit u (import xzy) (export)
    (+ x z y))
  
  (define-unit v (import) (export xzy)
    (define x 10)
    (define y 20)
    (define z 30))
  
  (test 60
        (invoke-unit (compound-unit/infer (import) (export) (link v u)))))

(define-signature s
  (x (define-values (y) (add1 x))))

(let ([x 1]
      [y 10]
      [s:x 100]
      [s:y 1000])
  (define-signature s2
    ((open (prefix s: s)) x (define-values (y) (sub1 x))))
  (define-unit u1 (import s2) (export)
    (list s:x s:y x y))
  (define-unit u2 (import) (export s2)
    (define s:x 3)
    (define x 19))
  (test '(3 4 19 18)
        (invoke-unit (compound-unit/infer (import) (export) (link u2 u1)))))


(define-signature sig^ (u-a))

(define-unit unit@
  (import)
  (export sig^)
  
  (define u-a 'zero))

(test 'zero
      (let ([q:u-a 5])
        (define-values/invoke-unit unit@ (import) (export (prefix q: sig^)))
        q:u-a))

(define-syntax (use-unit stx)
  (syntax-case stx ()
    [(_)
     #'(let ()
         (define-values/invoke-unit unit@ (import) (export sig^))
         u-a)]))

(define-syntax (use-unit2 stx)
  (syntax-case stx ()
    [(_)
     #'(let ()
         (define-values/invoke-unit/infer unit@)
         u-a)]))

(define-syntax (use-unit-badly1 stx)
  (syntax-case stx ()
    [(_ u-a)
     #'(let ()
         (define-values/invoke-unit unit@ (import) (export sig^))
         u-a)]))

(define-syntax (use-unit-badly2 stx)
  (syntax-case stx ()
    [(_ sig^)
     #'(let ()
         (define-values/invoke-unit unit@ (import) (export sig^))
         u-a)]))

(test 'zero (use-unit))
(test 'zero (use-unit2))
(test-runtime-error exn:fail:contract:variable? "context mismatch; no u-a"
  (use-unit-badly1 u-a))
(test-runtime-error exn:fail:contract:variable? "context mismatch; no u-a"
  (use-unit-badly2 sig^))

(test 12
      (let ()
        (define-signature s^ (x))
        (define-unit u@
          (import)
          (export s^)
          (define x 12))
        (define-values/invoke-unit u@ (import) (export s^))
        x))

;; ----------------------------------------

;; Make sure that right-hand side of a `define-values`
;; has the right scope, including in the case of
;; signature extension.
;; Based on examples from Dan Feltey.

(parameterize ([current-namespace (make-base-namespace)])
  (eval
   '(module scope-check/a-sig racket
      (provide a^)
      (define-signature a^ ((define-values (a) (+ b 1))))
      (define b 7)))
  (eval
   '(module scope-check/b-sig racket
      (require 'scope-check/a-sig)
      (provide result)

      (define-signature b^ extends a^ (b))

      (define b-out@ (unit (import) (export b^)
                           (define b "BAD")))
      (define b-in@
        (unit (import b^) (export) a))
      (define result
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((B : b^)) b-out@)
                              (() b-in@ B)))))))
  (test 8 (dynamic-require  ''scope-check/b-sig 'result)))

(parameterize ([current-namespace (make-base-namespace)])
  (eval
   '(module scope-check/a-sig racket
      (provide a^)
      (define-signature a^ ((define-values (a) (+ b 1))))
      (define b 7)))
  (eval
   '(module scope-check/b-sig racket
      (require 'scope-check/a-sig)
      (provide result)

      (define-signature b^ extends a^ ())
      (define b "BAD")

      (define b-out@ (unit (import) (export b^)))
      (define b-in@
        (unit (import b^) (export) a))
      (define result
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((B : b^)) b-out@)
                              (() b-in@ B)))))))
  (test 8 (dynamic-require  ''scope-check/b-sig 'result)))

;; ----------------------------------------

(module check-define-values-invoke-unit-spec racket/base
  (require racket/unit)

  (define-signature a^ (foo))
  (define-signature b^ (bar))

  (define-unit works@
    (import) (export a^) (define foo 'foo))
  (define-values/invoke-unit/infer
    (export (rename a^ [qux foo]))
    works@)

  (define-unit doesnt@
    (import) (export b^) (define bar 0))
  (define-unit work@
    (import b^) (export a^) (define foo bar))
  ;; No rename on export
  (define-values/invoke-unit/infer
    (export a^)
    (link doesnt@ work@))
  ;; Rename on export
  (define-values/invoke-unit/infer
    (export (rename a^ [baz foo]))
    (link doesnt@ work@))

  (provide results)
  (define results (list foo baz)))

(test '(0 0) (dynamic-require ''check-define-values-invoke-unit-spec 'results))

;; ----------------------------------------

(displayln "tests passed")
