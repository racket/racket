#lang racket/base
(require scribble/decode
         scribble/core
         rackunit
         racket/contract/base)

(check-true (pre-content? "x"))
(check-true (pre-content? null))
(check-true (pre-content? '("x")))
(check-true (pre-content? (list (element plain "x") "x")))
(check-true (pre-content? (splice (list (element plain "x") "x"))))
(check-true (not (pre-content? 11)))
(check-true (not (pre-content? '(11))))

(check-true (pre-flow? "x"))
(check-true (pre-flow? '("x")))
(check-true (pre-flow? (list (element plain "x") "x")))
(check-true (pre-flow? (list (paragraph plain (element plain "x")) (element plain "x") "x")))
(check-true (pre-flow? (splice (list (paragraph plain (element plain "x")) (element plain "x") "x"))))
(check-true (not (pre-flow? 11)))
(check-true (not (pre-flow? '(11))))

(check-true (pre-part? "x"))
(check-true (pre-part? '("x")))
(check-true (pre-part? (list (element plain "x") "x")))
(check-true (pre-part? (list (paragraph plain (element plain "x")) (element plain "x") "x")))
(check-true (pre-part? (list (paragraph plain (element plain "x")) (element plain "x") "x"
                             (decode-part '("x") null #f 0)
                             (part-start 0 #f null plain "start")
                             (part-index-decl '("a" "b") '("a"))
                             (part-collect-decl (element plain "x"))
                             (part-tag-decl '(something "x"))
                             (void)
                             (title-decl #f null #f plain "title"))))
(check-true (not (pre-part? 11)))
(check-true (not (pre-part? '(11))))

(check-true (part? (decode '("x"))))
(check-true (part? (decode-part '("x") null #f 0)))
(check-true (andmap block? (decode-flow '("x"))))
(check-true (block? (decode-compound-paragraph '("x"))))
(check-true (paragraph? (decode-paragraph '("x"))))
(check-true (content? (decode-content '("x"))))
(check-true (content? (decode-elements '("x"))))
(check-true (content? (decode-string "x")))

(check-true (whitespace? " \n\t\r"))
(check-true (not (whitespace? " \n\t\rx ")))

(check-equal? "a b c" (clean-up-index-string "  a  b   c   "))

(check-true (contract? (spliceof integer?)))
