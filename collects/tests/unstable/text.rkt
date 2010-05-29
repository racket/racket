#lang racket

(require rackunit rackunit/text-ui unstable/text "helpers.rkt")

(run-tests
 (test-suite "text.ss"
   (test-suite "text/c"
     (test-ok (with/c text/c "text"))
     (test-ok (with/c text/c #"text"))
     (test-ok (with/c text/c 'text))
     (test-ok (with/c text/c '#:text))
     (test-ok (with/c text/c #'"text"))
     (test-ok (with/c text/c #'#"text"))
     (test-ok (with/c text/c #'text))
     (test-ok (with/c text/c #'#:text))
     (test-bad (with/c text/c '(not text))))
   (test-suite "text?"
     (test-case "accept string"
       (check-pred text? "text"))
     (test-case "accept byte string"
       (check-pred text? #"text"))
     (test-case "accept symbol"
       (check-pred text? 'text))
     (test-case "accept keyword"
       (check-pred text? '#:text))
     (test-case "accept string literal"
       (check-pred text? #'"text"))
     (test-case "accept byte string literal"
       (check-pred text? #'#"text"))
     (test-case "accept identifier"
       (check-pred text? #'text))
     (test-case "accept keyword literal"
       (check-pred text? #'#:text))
     (test-case "reject non-text"
       (check-false (text? '(not text)))))
   (test-suite "string-literal?"
     (test-case "accept" (check-true (string-literal? #'"string")))
     (test-case "reject" (check-false (string-literal? "string"))))
   (test-suite "keyword-literal?"
     (test-case "accept" (check-true (keyword-literal? #'#:keyword)))
     (test-case "reject" (check-false (keyword-literal? '#:keyword))))
   (test-suite "bytes-literal?"
     (test-case "accept" (check-true (bytes-literal? #'#"bytes")))
     (test-case "reject" (check-false (bytes-literal? #"bytes"))))
   (test-suite "text=?"
     (test-case "string = string"
       (check text=? "abc" (string-copy "abc")))
     (test-case "string != string"
       (check-not text=? "abc" (string-copy "cba")))
     (test-case "string = identifier"
       (check text=? "car" #'car))
     (test-case "string != identifier"
       (check-not text=? "car" #'cdr))
     (test-case "identifier = identifier, different bindings"
       (check text=? #'car (datum->syntax #f 'car)))
     (test-case "identifier != identifier, no bindings"
       (check-not text=? #'UNBOUND (datum->syntax #f 'ALSO-UNBOUND))))
   (test-suite "text<?"
     (test-case "string < string"
       (check text<? "abc" "def"))
     (test-case "string !< string"
       (check-not text<? "abc" "abc"))
     (test-case "string < identifier"
       (check text<? "abc" #'def))
     (test-case "string !< identifier"
       (check-not text<? "abc" #'abc)))
   (test-suite "text<=?"
     (test-case "string <= string"
       (check text<=? "abc" "abc"))
     (test-case "string !<= string"
       (check-not text<=? "def" "abc"))
     (test-case "string <= identifier"
       (check text<=? "abc" #'abc))
     (test-case "string !<= identifier"
       (check-not text<=? "def" #'abc)))
   (test-suite "text>?"
     (test-case "string > string"
       (check text>? "def" "abc"))
     (test-case "string !> string"
       (check-not text>? "abc" "abc"))
     (test-case "string > identifier"
       (check text>? "def" #'abc))
     (test-case "string !> identifier"
       (check-not text>? "abc" #'abc)))
   (test-suite "text>=?"
     (test-case "string >= string"
       (check text>=? "abc" "abc"))
     (test-case "string !>= string"
       (check-not text>=? "abc" "def"))
     (test-case "string >= identifier"
       (check text>=? "abc" #'abc))
     (test-case "string !>= identifier"
       (check-not text>=? "abc" #'def)))
   (test-suite "text->string"
     (test-case "single" (check-equal? (text->string 'abc) "abc"))
     (test-case "multiple" (check-equal? (text->string 'a "b" #'c) "abc")))
   (test-suite "text->symbol"
     (test-case "single" (check-equal? (text->symbol "abc") 'abc))
     (test-case "multiple" (check-equal? (text->symbol 'a "b" #'c) 'abc)))
   (test-suite "text->keyword"
     (test-case "single" (check-equal? (text->keyword #'abc) '#:abc))
     (test-case "multiple" (check-equal? (text->keyword 'a "b" #'c) '#:abc)))
   (test-suite "text->bytes"
     (test-case "single" (check-equal? (text->bytes "abc") #"abc"))
     (test-case "multiple" (check-equal? (text->bytes 'a "b" #'c) #"abc")))
   (test-suite "text->identifier"
     (test-case "single, no context"
       (check-equal? (syntax-e (text->identifier "abc")) 'abc))
     (test-case "multiple w/ context"
       (check bound-identifier=?
              (text->identifier #:stx #'here 'a "b" #'c)
              #'abc)))
   (test-suite "text->string-literal"
     (test-case "single"
       (check-equal? (syntax-e (text->string-literal '#:abc)) "abc"))
     (test-case "multiple"
       (check-equal?
        (syntax-e (text->string-literal #:stx #'here 'a "b" #'c))
        "abc")))
   (test-suite "text->keyword-literal"
     (test-case "single"
       (check-equal? (syntax-e (text->keyword-literal #"abc")) '#:abc))
     (test-case "multiple"
       (check-equal?
        (syntax-e (text->keyword-literal #:stx #'here 'a "b" #'c))
        '#:abc)))
   (test-suite "text->bytes-literal"
     (test-case "single"
       (check-equal? (syntax-e (text->bytes-literal 'abc)) #"abc"))
     (test-case "multiple"
       (check-equal?
        (syntax-e (text->bytes-literal #:stx #'here 'a "b" #'c))
        #"abc")))))
