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

(define/with-syntax uu #'ABC)
(define/with-syntax (aa ...) #'(a b c))
(define/with-syntax (xx ...) #'(x y z))
(define/with-syntax (nn ...) #'(1 2 3))
(define/with-syntax ((yy ...) ...) #'((1 2 3) (4 5 6) (7 8 9)))

(define/syntax-parse (~or oo:nat _:id) #'x)
(define/syntax-parse ((~describe "x" (~or pp:nat _:id)) ...) #'(a 1 b 2 3))

;; ----------------------------------------

(tc (template uu) 'ABC)

;; FIXME: add other atoms when supported
;; FIXME: add other compound stx when supported
(tc (template abz) 'abz)
(tc (template ()) '())
(tc (template 5) '5)
(tc (template (1 2 #f #t "hey")) '(1 2 #f #t "hey"))
(tc (template (1 . b)) '(1 . b))
(tc (template (1 . uu)) '(1 . ABC))

(tc (template #(aa ... done))
    '#(a b c done))
(tc (template #s(blah xx ...))
    '#s(blah x y z))

(tc (template (aa ...))
    '(a b c))
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
