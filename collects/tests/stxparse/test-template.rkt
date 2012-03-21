#lang racket/base
(require (for-syntax racket/base)
         rackunit
         racket/syntax
         syntax/parse
         syntax/parse/experimental/template)

;; FIXME: need to test errors, too

(define-syntax (tc stx)
  (syntax-case stx ()
    [(tc expr expected)
     #`(test-equal? (format "line ~s" #,(syntax-line stx))
                    (syntax->datum expr)
                    expected)]))

;; ----------------------------------------

;; Common pattern variable definitions
;; (avoids having to have 'with-syntax' in every test case)

(define/with-syntax uu #'abc)
(define/with-syntax (aa ...) #'(a b c))
(define/with-syntax (xx ...) #'(x y z))
(define/with-syntax (nn ...) #'(1 2 3))
(define/with-syntax ((yy ...) ...) #'((1 2 3) (4 5 6) (7 8 9)))

(define/syntax-parse (~or oo:nat _:id) #'x)
(define/syntax-parse ((~describe "x" (~or pp:nat _:id)) ...) #'(a 1 b 2 3))

;; ----------------------------------------

(tc (template uu) 'abc)

;; FIXME: add other atoms when supported
;; FIXME: add other compound stx when supported
(tc (template abz) 'abz)
(tc (template ()) '())
(tc (template 5) '5)
(tc (template (1 2 #f #t "hey")) '(1 2 #f #t "hey"))
(tc (template (1 . b)) '(1 . b))
(tc (template (1 . uu)) '(1 . abc))

(tc (template #(aa ... done))
    '#(a b c done))
(tc (template #s(blah xx ...))
    '#s(blah x y z))

(tc (template (aa ...))
    '(a b c))
(tc (template ((uu aa) ...))
    '((abc a) (abc b) (abc c)))
(tc (template ((aa aa) ...))
    '((a a) (b b) (c c)))
(tc (template (start (aa ok) ... done))
    '(start (a ok) (b ok) (c ok) done))
(tc (template ((aa nn xx) ...))
    '((a 1 x) (b 2 y) (c 3 z)))
(tc (template (aa ... ((nn xx) ...)))
    '(a b c ((1 x) (2 y) (3 z))))
(tc (template (aa ... (nn xx) ...))
    '(a b c (1 x) (2 y) (3 z)))

(tc (template (aa ... ((yy ok) ...) ...))
    '(a b c ((1 ok) (2 ok) (3 ok)) ((4 ok) (5 ok) (6 ok)) ((7 ok) (8 ok) (9 ok))))

(tc (template ((?@ 1 2) 3))
    '(1 2 3))
(tc (with-syntax ([w '(1 2 3)])
      (template ((?@ 0 . w) 4)))
    '(0 1 2 3 4))
(tc (template ((?@ aa ok) ...))
    '(a ok b ok c ok))
(tc (template ((?@ aa nn) ...))
    '(a 1 b 2 c 3))
(tc (template (aa ... (?@ nn xx) ...))
    '(a b c 1 x 2 y 3 z))

;; escape
(tc (template (abc (xx (... (q ...))) ...))
    '(abc (x (q ...)) (y (q ...)) (z (q ...))))
(tc (template (abc (xx (... (q ... nn))) ...))
    '(abc (x (q ... 1)) (y (q ... 2)) (z (q ... 3))))

;; consecutive ellipses
(tc (template (yy ... ...))
    '(1 2 3 4 5 6 7 8 9))

;; ??
(tc (template (?? (ok oo go) nah))
    'nah)
(tc (template ((?? (ready oo)) done))
    '(done))

;; liberal depth rules

(tc (template (((uu aa yy) ...) ...))
    '(((abc a 1) (abc b 2) (abc c 3))
      ((abc a 4) (abc b 5) (abc c 6))
      ((abc a 7) (abc b 8) (abc c 9))))
(tc (template (((uu aa yy) ...) ...))
    ;; compatible with syntax
    (syntax->datum #'(((uu aa yy) ...) ...)))

;; liberal depth rules with consecutive ellipses

(tc (template ((aa yy) ... ...))
    '((a 1) (b 2) (c 3) (a 4) (b 5) (c 6) (a 7) (b 8) (c 9)))
(tc (template ((aa yy) ... ...))
    (syntax->datum #'((aa yy) ... ...)))

;; head ??

(tc (template ((?? (?@ #:yes uu) (?@ #:no)) done))
    '(#:yes abc done))
(tc (template ((?? (?@ #:yes oo) (?@ #:no)) done))
    '(#:no done))

(tc (template ((?? (?@ #:yes pp) (?@ #:no)) ...))
    '(#:no #:yes 1 #:no #:yes 2 #:yes 3))

;; ----------------------------------------

;; combined ?? ?@
(tc (syntax-parse #'(a b c 1 2 3)
      [(a:id ... (~optional s:str) n:nat ...)
       (template (a ... n ... (?@ . (?? (string: s) ()))))])
    '(a b c 1 2 3))
(tc (syntax-parse #'(a b c "hello!" 1 2 3)
      [(a:id ... (~optional s:str) n:nat ...)
       (template (a ... n ... (?@ . (?? (string: s) ()))))])
    '(a b c 1 2 3 string: "hello!"))

;; ----------------------------------------


(define-template-metafunction (join stx)
  (syntax-parse stx
    [(join a:id b:id ...)
     (datum->syntax #'a
                    (string->symbol
                     (apply string-append
                            (map symbol->string
                                 (syntax->datum #'(a b ...)))))
                    stx)]))

(tc (template (join a b c))
    'abc)
(tc (template ((xx (join tmp- xx)) ...))
    '((x tmp-x) (y tmp-y) (z tmp-z)))
(tc (template ((xx (join uu - xx)) ...))
    '((x abc-x) (y abc-y) (z abc-z)))
(tc (template ((xx (join aa xx)) ...))
    '((x ax) (y by) (z cz)))
