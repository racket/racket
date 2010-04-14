;; See csv.ss for legal information.
;; $Id: test-csv.ss,v 1.3 2009/02/24 05:17:26 neilpair Exp $

#lang scheme/base

(require (planet neil/testeez:1:1/testeez)
         "csv.ss")

(testeez
 "csv"

 (test-define "define an ascii CR char"
              cr
              (string (integer->char 13)))
 (test-define "define an ascii LF char"
              lf
              (string (integer->char 10)))

 (test/equal "simple"
             (csv->list (string-append
                         "a" lf "b" lf "c" lf "d" lf ""))
             '(("a") ("b") ("c") ("d")))

 (test/equal "simple"
             (csv->list (string-append "  a  "
                                       lf
                                       "  b  "
                                       lf
                                       "  c  "
                                       lf
                                       "  d  "
                                       lf
                                       ""))
             '(("  a  ") ("  b  ") ("  c  ") ("  d  ")))

 (test/equal "simple"
             (csv->list (string-append "aaa,bbb,ccc" cr lf
                                       "1,2,3" cr lf))
             '(("aaa" "bbb" "ccc") ("1" "2" "3")))

 (test/equal "quoted field"
             (csv->list "aaa,\"bbb\",ccc")
             '(("aaa" "bbb" "ccc")))

 (test/equal "quoted field with comma"
             (csv->list "aaa,\"bbb,bbb\",ccc")
             '(("aaa" "bbb,bbb" "ccc")))

 (test/equal "quoted field followed by whitespace"
             (csv->list "aaa,\"bbb\"   ,ccc")
             '(("aaa" "bbb" "ccc")))

 (test/equal "quoted field with newline in it"
             (csv->list (string-append "aaa,\"b" lf "b\",ccc" lf
                                       "ddd,eee,fff" lf))
             `(("aaa" ,(string-append "b" lf "b") "ccc")
               ("ddd" "eee" "fff")))

 (test/equal "quoted field with doubling escape in middle"
             (csv->list "aaa,\"b\"\"b\",ccc")
             '(("aaa" "b\"b" "ccc")))

 (test/equal "quoted field with doubling escape at beginning"
             (csv->list "aaa,\"\"\"bbb\",ccc")
             '(("aaa" "\"bbb" "ccc")))

 (test/equal "quoted field with doubling escape at end"
             (csv->list "aaa,\"bbb\"\"\",ccc")
             '(("aaa" "bbb\"" "ccc")))

 (test/equal "quoted field with unterminated quote"
             (csv->list "aaa,\"bbb,ccc")
             '(("aaa" "bbb,ccc")))

 (test/equal "quoted field followed by eof"
             (csv->list "aaa,\"bbb\"")
             '(("aaa" "bbb")))

 (test-define "define a reader-maker that strips whitespace"
              make-ws-stripping-reader
              (make-csv-reader-maker
               '((strip-leading-whitespace?  . #t)
                 (strip-trailing-whitespace? . #t))))

 (test/equal "whitespace strip on simple row terminated by eof"
             (csv->list (make-ws-stripping-reader
                         "  a  ,  b  ,  c  "))
             '(("a" "b" "c")))

 (test-define "define a newline-adapting reader-maker"
              make-nl-adapt-reader
              (make-csv-reader-maker '((newline-type . adapt))))

 (test/equal "try newline-adapting reader-maker first time"
             (csv->list (make-nl-adapt-reader
                         (string-append "aaa,bbb" lf
                                        "ccc" cr ",ddd" cr lf
                                        "eee,fff")))
             `(("aaa" "bbb")
               (,(string-append "ccc" cr)
                ,(string-append "ddd" cr))
               ("eee" "fff")))

 (test/equal "try newline-adapting reader-maker second time"
             (csv->list (make-nl-adapt-reader
                         (string-append "aaa,bbb" cr lf
                                        "ccc" cr ",ddd" lf cr lf
                                        "eee,fff" cr lf)))
             `(("aaa" "bbb")
               (,(string-append "ccc" cr)
                ,(string-append "ddd" lf))
               ("eee" "fff")))

 (test-define "define an input string with pound char"
              str
              (string-append "a,b,c"  lf
                             "#d,e,f"  lf
                             "g,h,i"  lf))

 (test-define "define reader-maker with pound as comment char"
              make-reader-with-pound-quote
              (make-csv-reader-maker '((comment-chars . (#\#)))))

 (test/equal "read str without pound as comment char"
             (csv->list str)
             '(("a" "b" "c") ("#d" "e" "f") ("g" "h" "i")))

 (test/equal "read str with pound as comment char"
             (csv->list (make-reader-with-pound-quote str))
             '(("a" "b" "c") ("g" "h" "i")))

 (test/equal "csv->sxml without row and column names"
             (csv->sxml (string-append "aaa,bbb,ccc" cr lf
                                       "1,2,3" cr lf))
             `(,(string->symbol "*TOP*")
               (row (col-0 "aaa") (col-1 "bbb") (col-2 "ccc"))
               (row (col-0 "1")   (col-1 "2")   (col-2 "3"))))

 (test/equal "csv->sxml with row and column names"
             (csv->sxml (string-append "aaa,bbb,ccc" cr lf
                                       "1,2,3" cr lf)
                        'foo
                        '(first second third))
             `(,(string->symbol "*TOP*")
               (foo (first "aaa") (second "bbb") (third "ccc"))
               (foo (first "1")   (second "2")   (third "3"))))

 ;; TODO: Add more test cases.

 )
