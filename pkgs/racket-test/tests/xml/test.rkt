#lang racket
(require rackunit
         rackunit/text-ui
         xml
         xml/plist
         mzlib/etc
         "to-list.rkt")

;; test-bad-read-input : format-str str -> void
;; First argument is the input, second is the error message
(define ((mk-test-read-xml/exn read-xml) format-str err-string)
  (define str (format format-str))
  (test-exn
   str
   (lambda (x)
     (regexp-match (regexp-quote err-string) (exn-message x)))
   (lambda ()
     (read-xml (open-input-string str)))))

(define test-read-xml/exn (mk-test-read-xml/exn read-xml))
(define (test-read-xml str xml #:document-different? [diff? #f])
  (unless diff?
   (test-equal? str (document->list (read-xml/document (open-input-string str))) xml))
  (test-equal? str (document->list (read-xml (open-input-string str))) xml))

(define test-syntax:read-xml/exn (mk-test-read-xml/exn syntax:read-xml))
(define (test-syntax:read-xml str xml)
  (test-equal? str (syntax->datum (syntax:read-xml (open-input-string str))) xml))

(define test-read-xml/element/exn (mk-test-read-xml/exn read-xml/element))
(define (test-read-xml/element str xml)
  (test-equal? str (element->list (read-xml/element (open-input-string str))) xml))

(define test-syntax:read-xml/element/exn (mk-test-read-xml/exn syntax:read-xml/element))
(define (test-syntax:read-xml/element str xml)
  (test-equal? str (syntax->datum (syntax:read-xml/element (open-input-string str))) xml))

(define (test-write-xml str)
  (test-equal? str (with-output-to-string (lambda () (write-xml (read-xml (open-input-string str))))) str))
(define (test-write-xml/content str)
  (test-equal? str (with-output-to-string (lambda () (write-xml/content (document-element (read-xml (open-input-string str)))))) str))

(define (test-display-xml str res)
  (test-equal? str (with-output-to-string (lambda () (display-xml (read-xml (open-input-string str))))) res))
(define (test-display-xml/content str res)
  (test-equal? str (with-output-to-string (lambda () (display-xml/content (document-element (read-xml (open-input-string str)))))) res))

(define (test-xexpr? xe)
  (test-not-false (format "~S" xe) (xexpr? xe)))
(define (test-not-xexpr? xe)
  (test-false (format "~S" xe) (xexpr? xe)))

(define (contract->predicate c)
  (lambda (v)
    (with-handlers ([exn:fail:contract?
                     (lambda (x) #f)])
      (contract c v 'pos 'neg)
      #t)))    

(define xml-tests
  (test-suite
   "XML"
   #:before (λ () (empty-tag-shorthand 'always))
   
   (test-suite
    "Legacy tests"   
    
    (test-suite 
     "DOCTYPE"
     
     (let ()
       (define source-string #<<END
<!DOCTYPE html PUBLIC
 "-//W3C//DTD XHTML 1.0 Transitional//EN"
 "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"> </html>
END
         )
       
       (define source-document
         (read-xml (open-input-string source-string)))
       (define result-string
         (with-output-to-string (lambda () (write-xml source-document))))
       (define expected-string #<<END
<html xmlns="http://www.w3.org/1999/xhtml"> </html>
END
         )
       (test-equal?
        "DOCTYPE dropping" result-string expected-string)))
    
    (local 
      [(define a-pi (make-p-i #f #f 'foo "bar"))
       (define a-p (make-prolog empty #f empty))
       (define a-p/pi (make-prolog (list a-pi) #f (list)))
       (define a-d0
         (make-document a-p (make-element #f #f 'html empty empty)
                        empty))
       (define a-d1
         (make-document a-p (make-element #f #f 'html empty empty)
                        (list a-pi)))
       (define a-d2
         (make-document a-p/pi (make-element #f #f 'html empty empty)
                        (list a-pi)))]
      (test-suite 
       "PIs"
       (test-equal? "Display XML w/o pis"
                    (with-output-to-string (lambda () (display-xml a-d0)))
                    "\n<html />")
       (test-equal? "Display XML w/ pi in doc-misc"
                    (with-output-to-string (lambda () (display-xml a-d1)))
                    "\n<html /><?foo bar?>\n")
       (test-equal? "Display XML w/ pi in doc-misc and prolog"
                    (with-output-to-string (lambda () (display-xml a-d2)))
                    "<?foo bar?>\n\n<html /><?foo bar?>\n"))))
   
   (test-suite
    "Datatypes"
    (test-suite 
     "xexpr"
     (test-xexpr? "string")
     (test-xexpr? (list 'a (list (list 'href "#")) "content"))
     (test-xexpr? (list 'p "one" "two" "three"))
     (test-xexpr? 'nbsp)
     (test-xexpr? 10)
     (test-not-xexpr? 0)
     (test-not-xexpr? '(a ((b)) c))
     (test-xexpr? (make-cdata #f #f "unquoted <b>"))
     (test-xexpr? (make-comment "Comment!"))
     (test-xexpr? (make-pcdata #f #f "quoted <b>"))
     
     (test-not-xexpr? (list 'a (list (list 'href)) "content"))
     
     (test-not-xexpr? +)
     (test-not-xexpr? #f)
     (test-not-xexpr? '()))
    
    (test-not-false "xexpr/c" (contract? xexpr/c))
    
    (test-not-false "document" (document? (make-document (make-prolog empty #f empty) (make-element #f #f 'br empty empty) empty)))
    
    (test-not-false "prolog" (prolog? (make-prolog empty #f empty)))
    (let ([c1 (make-comment "c1")]
          [c2 (make-comment "c2")])
      (test-equal? "prolog" (prolog-misc2 (make-prolog empty #f (list c1 c2)))
                   (list c1 c2)))
    
    (test-not-false "document-type" (document-type? (make-document-type 'name (make-external-dtd "string") #f)))
    
    (test-not-false "external-dtd" (external-dtd? (make-external-dtd "string")))
    (test-not-false "external-dtd/public" (external-dtd/public? (make-external-dtd/public "string" "public")))
    (test-not-false "external-dtd/system" (external-dtd/system? (make-external-dtd/system "string")))
    
    (test-not-false "element" (element? (make-element #f #f 'br empty empty)))
    
    (local [(define content? (contract->predicate content/c))]
      (test-suite
       "content?"
       (test-not-false "content? pcdata" (content? (make-pcdata #f #f "pcdata")))
       (test-not-false "content? element" (content? (make-element #f #f 'br empty empty)))
       (test-not-false "content? entity" (content? (make-entity #f #f 'nbsp)))
       (test-not-false "content? comment" (content? (make-comment "string")))
       (test-not-false "content? cdata" (content? (make-cdata #f #f "cdata")))))
    
    (test-not-false "attribute" (attribute? (make-attribute #f #f 'name "value")))
    
    (test-not-false "entity symbol" (entity? (make-entity #f #f 'nbsp)))
    (test-not-false "entity number" (entity? (make-entity #f #f 10)))
    
    (test-not-false "pcdata" (pcdata? (make-pcdata #f #f "string")))
    
    (test-not-false "cdata" (cdata? (make-cdata #f #f "string")))
    
    (test-not-false "p-i" (p-i? (make-p-i #f #f 'target "instruction")))
    
    (test-not-false "comment" (comment? (make-comment "text")))
    
    (test-not-false "source" (source? (make-source 'start 'stop)))
    (test-not-false "source" (source? (make-source (make-location 1 2 3) 'stop)))
    (test-not-false "source" (source? (make-source 'start (make-location 1 2 3))))
    (test-not-false "source" (source? (make-source (make-location 1 2 3) (make-location 4 5 6)))))
   
   (test-suite
    "Reading and Writing XML"
    
    (test-suite
     "read-xml"
     (test-read-xml/exn "<" "read-xml: lex-error: at position 1.1/2: unexpected eof")
     (test-read-xml/exn "<a>" "read-xml: parse-error: unclosed `a' tag at [1.0/1 1.3/4]")
     (test-read-xml/exn
      "<a></b>"
      "read-xml: parse-error: start tag `a' at [1.0/1 1.3/4] doesn't match end tag `b' at [1.3/4 1.7/8]")
     (test-read-xml/exn
      "<a <a>" "read-xml: lex-error: at position 1.4/5: expected / or > to close tag `a'")
     
     (test-read-xml/exn "~n<" "read-xml: lex-error: at position 2.1/3: unexpected eof")
     (test-read-xml/exn "~n<a>" "read-xml: parse-error: unclosed `a' tag at [2.0/2 2.3/5]")
     (test-read-xml/exn
      "~n<a></b>"
      "read-xml: parse-error: start tag `a' at [2.0/2 2.3/5] doesn't match end tag `b' at [2.3/5 2.7/9]")
     (test-read-xml/exn
      "~n<a <a>" "read-xml: lex-error: at position 2.4/6: expected / or > to close tag `a'")
     
     (test-read-xml/exn "" "read-xml: parse-error: expected root element - received #<eof>")
     (test-read-xml/exn "<br /><br />" "read-xml: parse-error: extra stuff at end of document (element ")
     
     (test-read-xml 
      "<doc><bold>hi</bold> there!</doc>"
      '(make-document
        (make-prolog (list) #f (list))
        (make-element
         (make-source (make-location 1 0 1) (make-location 1 33 34))
         'doc
         (list)
         (list
          (make-element
           (make-source (make-location 1 5 6) (make-location 1 20 21))
           'bold
           (list)
           (list (make-pcdata (make-source (make-location 1 11 12) (make-location 1 13 14)) "hi")))
          (make-pcdata (make-source (make-location 1 20 21) (make-location 1 27 28)) " there!")))
        (list)))
     
     (test-read-xml
      "<a href=\"#\">inner</a>"
      '(make-document
        (make-prolog (list) #f (list))
        (make-element
         (make-source (make-location 1 0 1) (make-location 1 21 22))
         'a
         (list (make-attribute (make-source (make-location 1 3 4) (make-location 1 11 12)) href "#"))
         (list (make-pcdata (make-source (make-location 1 12 13) (make-location 1 17 18)) "inner")))
        (list)))
     
     (test-read-xml
      "<root>&nbsp;</root>"
      '(make-document
        (make-prolog (list) #f (list))
        (make-element
         (make-source (make-location 1 0 1) (make-location 1 19 20))
         'root
         (list)
         (list (make-entity (make-source (make-location 1 6 7) (make-location 1 12 13)) 'nbsp)))
        (list)))
     
     (test-read-xml
      "<root>&#40;</root>"
      '(make-document
        (make-prolog (list) #f (list))
        (make-element
         (make-source (make-location 1 0 1) (make-location 1 18 19))
         'root
         (list)
         (list (make-entity (make-source (make-location 1 6 7) (make-location 1 11 12)) '40)))
        (list)))
     
     (test-read-xml/exn
      "<root>&#0;</root>"
      "read-xml: lex-error: at position 1.10/11: not a well-formed numeric entity (does not match the production for Char, see XML 4.1)")
     
     (test-read-xml
      "<!-- comment --><br />"
      '(make-document
        (make-prolog (list) #f (list))
        (make-element (make-source (make-location 1 16 17) (make-location 1 22 23)) 'br (list) (list))
        (list)))

     (test-read-xml
      "<?xml version=\"1.0\"? encoding=\"UTF-8\" standalone=\"yes\"?><br />"
      '(make-document
        (make-prolog
         (list
          (make-p-i
           (make-source (make-location 1 0 1) (make-location 1 56 57))
           xml
           "version=\"1.0\"? encoding=\"UTF-8\" standalone=\"yes\""))
         #f
         (list))
        (make-element
         (make-source (make-location 1 56 57) (make-location 1 62 63))
         'br
         (list)
         (list))
        (list)))

     (test-read-xml #:document-different? #t
      "<br /><?xml version=\"1.0\"? encoding=\"UTF-8\" standalone=\"yes\"?>"
      '(make-document
        (make-prolog (list) #f (list))
        (make-element
         (make-source (make-location 1 0 1) (make-location 1 6 7))
         'br
         (list)
         (list))
        (list
         (make-p-i
          (make-source (make-location 1 6 7) (make-location 1 62 63))
          xml
          "version=\"1.0\"? encoding=\"UTF-8\" standalone=\"yes\""))))
     
     ; XXX need more read-xml tests
     
     )
    
    (test-suite
     "read-xml/element"
     (test-read-xml/element/exn "<" "read-xml: lex-error: at position 1.1/2: unexpected eof")
     (test-read-xml/element/exn "<a>" "read-xml: parse-error: unclosed `a' tag at [1.0/1 1.3/4]")
     (test-read-xml/element/exn
      "<a></b>"
      "read-xml: parse-error: start tag `a' at [1.0/1 1.3/4] doesn't match end tag `b' at [1.3/4 1.7/8]")
     (test-read-xml/element/exn
      "<a <a>" "read-xml: lex-error: at position 1.4/5: expected / or > to close tag `a'")
     
     (test-read-xml/element/exn "~n<" "read-xml: lex-error: at position 2.1/3: unexpected eof")
     (test-read-xml/element/exn "~n<a>" "read-xml: parse-error: unclosed `a' tag at [2.0/2 2.3/5]")
     (test-read-xml/element/exn
      "~n<a></b>"
      "read-xml: parse-error: start tag `a' at [2.0/2 2.3/5] doesn't match end tag `b' at [2.3/5 2.7/9]")
     (test-read-xml/element/exn
      "~n<a <a>" "read-xml: lex-error: at position 2.4/6: expected / or > to close tag `a'")
     
     (test-read-xml/element/exn "" "read-xml: parse-error: expected root element - received #<eof>")
     
     (test-read-xml/element
      "<br /><br />"
      '(make-element (make-source (make-location 1 0 1) (make-location 1 6 7)) 'br (list) (list)))
     
     (test-read-xml/element 
      "<doc><bold>hi</bold> there!</doc>"
      '(make-element
        (make-source (make-location 1 0 1) (make-location 1 33 34))
        'doc
        (list)
        (list
         (make-element
          (make-source (make-location 1 5 6) (make-location 1 20 21))
          'bold
          (list)
          (list (make-pcdata (make-source (make-location 1 11 12) (make-location 1 13 14)) "hi")))
         (make-pcdata (make-source (make-location 1 20 21) (make-location 1 27 28)) " there!"))))
     
     (test-read-xml/element
      "<a href=\"#\">inner</a>"
      '(make-element
        (make-source (make-location 1 0 1) (make-location 1 21 22))
        'a
        (list (make-attribute (make-source (make-location 1 3 4) (make-location 1 11 12)) href "#"))
        (list (make-pcdata (make-source (make-location 1 12 13) (make-location 1 17 18)) "inner"))))
     
     (test-read-xml/element
      "<root>&nbsp;</root>"
      '(make-element
        (make-source (make-location 1 0 1) (make-location 1 19 20))
        'root
        (list)
        (list (make-entity (make-source (make-location 1 6 7) (make-location 1 12 13)) 'nbsp))))
     
     (test-read-xml/element
      "<root>&#40;</root>"
      '(make-element
        (make-source (make-location 1 0 1) (make-location 1 18 19))
        'root
        (list)
        (list (make-entity (make-source (make-location 1 6 7) (make-location 1 11 12)) '40))))
     
     (test-read-xml/element/exn
      "<!-- comment --><br />"
      "read-xml: parse-error: expected root element - received (comment ")
     
     (test-read-xml/element
      "<title><![CDATA[hello world[mp3]]]></title>"
      '(make-element
        (make-source (make-location 1 0 1) (make-location 1 43 44))
        'title
        (list)
        (list (make-cdata (make-source (make-location 1 7 8) (make-location 1 35 36)) "<![CDATA[hello world[mp3]]]>"))))
     
     (test-read-xml/element
      "<title><![CDATA[]]]></title>"
      '(make-element
        (make-source (make-location 1 0 1) (make-location 1 28 29))
        'title
        (list)
        (list (make-cdata (make-source (make-location 1 7 8) (make-location 1 20 21)) "<![CDATA[]]]>"))))
     
     ; XXX need more read-xml/element tests
     
     )
    
    (test-suite
     "syntax:read-xml"
     (test-syntax:read-xml/exn "<" "read-xml: lex-error: at position 1.1/2: unexpected eof")
     (test-syntax:read-xml/exn "<a>" "read-xml: parse-error: unclosed `a' tag at [1.0/1 1.3/4]")
     (test-syntax:read-xml/exn
      "<a></b>"
      "read-xml: parse-error: start tag `a' at [1.0/1 1.3/4] doesn't match end tag `b' at [1.3/4 1.7/8]")
     (test-syntax:read-xml/exn
      "<a <a>" "read-xml: lex-error: at position 1.4/5: expected / or > to close tag `a'")
     
     (test-syntax:read-xml/exn "~n<" "read-xml: lex-error: at position 2.1/3: unexpected eof")
     (test-syntax:read-xml/exn "~n<a>" "read-xml: parse-error: unclosed `a' tag at [2.0/2 2.3/5]")
     (test-syntax:read-xml/exn
      "~n<a></b>"
      "read-xml: parse-error: start tag `a' at [2.0/2 2.3/5] doesn't match end tag `b' at [2.3/5 2.7/9]")
     (test-syntax:read-xml/exn
      "~n<a <a>" "read-xml: lex-error: at position 2.4/6: expected / or > to close tag `a'")
     
     (test-syntax:read-xml/exn "" "read-xml: parse-error: expected root element - received #<eof>")
     (test-syntax:read-xml/exn "<br /><br />" "read-xml: parse-error: extra stuff at end of document (element ")
     
     (test-syntax:read-xml 
      "<doc><bold>hi</bold> there!</doc>"
      '(doc () (bold () "hi") " there!"))
     
     (test-syntax:read-xml
      "<a href=\"#\">inner</a>"
      '(a ([href "#"]) "inner"))
     
     (test-syntax:read-xml
      "<root>&nbsp;</root>"
      '(root () nbsp))
     
     (test-syntax:read-xml
      "<root>&#40;</root>"
      '(root () 40))
     
     (test-syntax:read-xml
      "<!-- comment --><br />"
      '(br ()))
     
     ; XXX need more syntax:read-xml tests
     
     )
    
    (test-suite
     "syntax:read-xml/element"
     (test-syntax:read-xml/element/exn "<" "read-xml: lex-error: at position 1.1/2: unexpected eof")
     (test-syntax:read-xml/element/exn "<a>" "read-xml: parse-error: unclosed `a' tag at [1.0/1 1.3/4]")
     (test-syntax:read-xml/element/exn
      "<a></b>"
      "read-xml: parse-error: start tag `a' at [1.0/1 1.3/4] doesn't match end tag `b' at [1.3/4 1.7/8]")
     (test-syntax:read-xml/element/exn
      "<a <a>" "read-xml: lex-error: at position 1.4/5: expected / or > to close tag `a'")
     
     (test-syntax:read-xml/element/exn "~n<" "read-xml: lex-error: at position 2.1/3: unexpected eof")
     (test-syntax:read-xml/element/exn "~n<a>" "read-xml: parse-error: unclosed `a' tag at [2.0/2 2.3/5]")
     (test-syntax:read-xml/element/exn
      "~n<a></b>"
      "read-xml: parse-error: start tag `a' at [2.0/2 2.3/5] doesn't match end tag `b' at [2.3/5 2.7/9]")
     (test-syntax:read-xml/element/exn
      "~n<a <a>" "read-xml: lex-error: at position 2.4/6: expected / or > to close tag `a'")
     
     (test-syntax:read-xml/element/exn "" "read-xml: parse-error: expected root element - received #<eof>")
     (test-syntax:read-xml/element 
      "<br /><br />"
      '(br ()))
     
     (test-syntax:read-xml/element 
      "<doc><bold>hi</bold> there!</doc>"
      '(doc () (bold () "hi") " there!"))
     
     (test-syntax:read-xml/element
      "<a href=\"#\">inner</a>"
      '(a ([href "#"]) "inner"))
     
     (test-syntax:read-xml/element
      "<root>&nbsp;</root>"
      '(root () nbsp))
     
     (test-syntax:read-xml/element
      "<root>&#40;</root>"
      '(root () 40))
     
     (test-syntax:read-xml/element/exn
      "<!-- comment --><br />"
      "read-xml: parse-error: expected root element - received (comment ")
     
     ; XXX need more syntax:read-xml/element tests
     
     )
    
    (test-suite
     "write-xml"
     (test-write-xml "<doc><bold>hi</bold> there!</doc>")
     (test-write-xml "<a href=\"#\">inner</a>")
     (test-write-xml "<root>&nbsp;</root>")
     (test-write-xml "<root>&#40;</root>")
     (test-write-xml "<br />")
     ; XXX need more write-xml tests
     )
    
    (test-suite
     "write-xml/content"
     (test-write-xml/content "<doc><bold>hi</bold> there!</doc>")
     (test-write-xml/content "<a href=\"#\">inner</a>")
     (test-write-xml/content "<root>&nbsp;</root>")
     (test-write-xml/content "<root>&#40;</root>")
     (test-write-xml/content "<br />")
     ; XXX need more write-xml/content tests
     )
    
    (test-suite
     "display-xml"
     (test-display-xml "<doc><bold>hi</bold> there!</doc>" "\n<doc>\n  <bold>\n    hi\n  </bold>\n   there!\n</doc>")
     (test-display-xml "<a href=\"#\">inner</a>" "\n<a href=\"#\">\n  inner\n</a>")
     (test-display-xml "<root>&nbsp;</root>" "\n<root>&nbsp;\n</root>")
     (test-display-xml "<root>&#40;</root>" "\n<root>&#40;\n</root>")
     (test-display-xml "<br />" "\n<br />")
     ; XXX need more display-xml tests
     )
    
    (test-suite
     "display-xml/content"
     (test-display-xml/content "<doc><bold>hi</bold> there!</doc>" "\n<doc>\n  <bold>\n    hi\n  </bold>\n   there!\n</doc>")
     (test-display-xml/content "<a href=\"#\">inner</a>" "\n<a href=\"#\">\n  inner\n</a>")
     (test-display-xml/content "<root>&nbsp;</root>" "\n<root>&nbsp;\n</root>")
     (test-display-xml/content "<root>&#40;</root>" "\n<root>&#40;\n</root>")
     (test-display-xml/content "<br />" "\n<br />")
     ; XXX need more display-xml/content tests
     )
    )
   
   (local
     [(define (test-xml->xexpr str xe)
        (test-equal? str (string->xexpr str) xe))
      (define (test-xexpr->string xe str)
        (test-equal? (format "~S" xe) (xexpr->string xe) str)
        (test-equal? (format "~S" xe) (with-output-to-string (λ () (write-xexpr xe))) str)
        (test-string->xexpr str xe))
      (define (test-string->xexpr str xe)
        (test-equal? str (string->xexpr str) xe))]
     (test-suite 
      "XML and X-expression Conversions"
      
      (test-suite
       "xml->xexpr"
       (test-xml->xexpr    
        "<doc><bold>hi</bold> there!</doc>"
        '(doc () (bold () "hi") " there!"))
       
       (test-xml->xexpr
        "<a href=\"#\">inner</a>"
        '(a ([href "#"]) "inner"))
       
       (test-xml->xexpr
        "<root>&nbsp;</root>"
        '(root () nbsp))
       
       (test-xml->xexpr
        "<root>&#40;</root>"
        '(root () 40))
       
       ; XXX more xml->xexpr tests
       )
      
      (test-suite
       "xexpr->string"
       (test-xexpr->string '(doc () (bold () "hi") " there!")
                           "<doc><bold>hi</bold> there!</doc>")
       (test-xexpr->string '(a ([href "#"]) "inner")
                           "<a href=\"#\">inner</a>")
       (test-xexpr->string '(root () nbsp)
                           "<root>&nbsp;</root>")      
       (test-xexpr->string '(root () 40)
                           "<root>&#40;</root>")
       (test-xexpr->string '(root () "\f")
                           "<root>\f</root>")
       ; XXX more xexpr->string tests
       )
      
      (test-suite
       "string->xexpr"
       (test-string->xexpr "<?foo bar?>\n\n<html /><?foo bar?>\n"
                           '(html ()))
       (parameterize ([xexpr-drop-empty-attributes #t])
         (test-string->xexpr "<?foo bar?>\n\n<html /><?foo bar?>\n"
                             '(html))))
      
      (local
        [(define (test-eliminate-whitespace tags choose str res)
           (test-equal? (format "~S" (list tags choose str))
                        (with-output-to-string 
                            (lambda () 
                              (write-xml/content ((eliminate-whitespace tags choose) (read-xml/element (open-input-string str))))))
                        res))
         (define (test-eliminate-whitespace/exn tags choose str msg)
           (test-exn (format "~S" (list tags choose str))
                     (lambda (x)
                       (and (exn? x)
                            (regexp-match (regexp-quote msg) (exn-message x))))
                     (lambda ()
                       (with-output-to-string 
                           (lambda () 
                             (write-xml/content ((eliminate-whitespace tags choose) (read-xml/element (open-input-string str)))))))))
         (define (truer x) #t)]
        (test-suite             
         "eliminate-whitespace"
         
         (test-equal? "Defaults"
                      (with-output-to-string 
                          (lambda () 
                            (write-xml/content ((eliminate-whitespace) (read-xml/element (open-input-string "<html>\n<p>Hey</p></html>"))))))
                      "<html>\n<p>Hey</p></html>")
         
         (test-eliminate-whitespace empty identity "<html>\n<p>Hey</p></html>" "<html>\n<p>Hey</p></html>")
         (test-eliminate-whitespace/exn empty not "<html>\n<p>Hey</p></html>" "not allowed to contain text")
         (test-eliminate-whitespace/exn empty truer "<html>\n<p>Hey</p></html>" "not allowed to contain text")
         
         (test-eliminate-whitespace '(html) identity "<html>\n<p>Hey</p></html>" "<html><p>Hey</p></html>")
         (test-eliminate-whitespace/exn '(html) not "<html>\n<p>Hey</p></html>" "not allowed to contain text")
         (test-eliminate-whitespace/exn '(html) truer "<html>\n<p>Hey</p></html>" "not allowed to contain text")
         
         (test-eliminate-whitespace '(html) identity "<html>\n<p>\n</p></html>" "<html><p>\n</p></html>")
         (test-eliminate-whitespace '(html) not "<html>\n<p>\n</p></html>" "<html>\n<p /></html>")
         (test-eliminate-whitespace '(html) truer "<html>\n<p>\n</p></html>" "<html><p /></html>")))
      
      (local
        [(define (test-validate-xexpr xe)
           (test-not-false (format "~S" xe) (validate-xexpr xe)))
         (define (test-validate-xexpr/exn xe v)
           (test-exn (format "~S" xe)
                     (lambda (x)
                       (and (exn:invalid-xexpr? x)
                            (equal? (exn:invalid-xexpr-code x) v)))
                     (lambda ()
                       (validate-xexpr xe))))]
        (test-suite
         "validate-xexpr"
         (test-validate-xexpr 64)
         (test-validate-xexpr 'nbsp)
         (test-validate-xexpr "string")
         (test-validate-xexpr (make-pcdata #f #f "pcdata"))
         (test-validate-xexpr (make-cdata #f #f "cdata"))
         (test-validate-xexpr (make-comment "comment"))
         (test-validate-xexpr (make-p-i #f #f 's1 "s2"))
         (test-validate-xexpr '(br))
         (test-validate-xexpr '(br ()))
         (test-validate-xexpr '(a ([href "#"]) "string"))
         
         (test-validate-xexpr/exn #f #f)
         (test-validate-xexpr 4)
         (test-validate-xexpr/exn + +)
         (test-validate-xexpr/exn '(a ([href foo]) bar) 'foo)
         (test-validate-xexpr/exn '("foo" bar) '("foo" bar))
         (test-validate-xexpr/exn '(x (("not-a-symbol" "42")))
                                  "not-a-symbol")
         (test-validate-xexpr/exn '(x (("also-not-a-symbol")))
                                  "also-not-a-symbol")))
      
      (test-suite
       "correct-xexpr?"
       (parameterize ([permissive-xexprs #f])
         (test-equal? "null is not an xexpr"
                      (correct-xexpr? '() (lambda () 'no) (lambda (exn) 'yes))
                      'yes)
         (test-true "malformed xexpr"
                      (correct-xexpr? '(a ((b)) c) 
                                      (lambda () #f)
                                      (lambda (exn) #t)))))

      ; XXX correct-xexpr?
      
      (test-suite
       "permissive-xexprs"
       (test-exn
        "Non-permissive"
        (lambda (exn)
          (and (exn? exn)
               (regexp-match #rx"not in permissive mode" (exn-message exn))))
        (lambda ()
          (xml->xexpr #f)))
       
       (test-false
        "Permissive"
        (parameterize ([permissive-xexprs #t])
          (xml->xexpr #f))))))
   
   (local
     [(define ((mk-test-param param) v istr ostr)
        (test-equal? (format "~S" (list v istr))
                     (parameterize ([param v])
                       (with-output-to-string
                           (lambda ()
                             (write-xml (read-xml (open-input-string istr))))))
                     ostr))
      (define test-empty-tag-shorthand (mk-test-param empty-tag-shorthand))
      (define test-collapse-whitespace (mk-test-param collapse-whitespace))
      (define test-read-comments (mk-test-param read-comments))]
     (test-suite
      "Parameters"
      
      (test-suite
       "empty-tag-shorthand"
       (test-empty-tag-shorthand 'always "<html></html>" "<html />")
       (test-empty-tag-shorthand 'always "<html>Hey</html>" "<html>Hey</html>")
       (test-empty-tag-shorthand 'never "<html></html>" "<html></html>")
       (test-empty-tag-shorthand 'never "<html>Hey</html>" "<html>Hey</html>")
       (test-empty-tag-shorthand empty "<html></html>" "<html></html>")
       (test-empty-tag-shorthand empty "<html>Hey</html>" "<html>Hey</html>")
       (test-empty-tag-shorthand '(html) "<html></html>" "<html />")
       (test-empty-tag-shorthand '(html) "<html>Hey</html>" "<html>Hey</html>")
       (test-empty-tag-shorthand '(p) "<html></html>" "<html></html>")
       (test-empty-tag-shorthand '(p) "<html>Hey</html>" "<html>Hey</html>"))
      
      (test-equal? "html-empty-tags"
                   html-empty-tags
                   '(param meta link isindex input img hr frame col br basefont base area))
      
      (test-suite
       "collapse-whitespace"
       (test-collapse-whitespace #t "<html>\n</html>" "<html> </html>")
       (test-collapse-whitespace #t "<html>\t</html>" "<html> </html>")
       (test-collapse-whitespace #t "<html>    </html>" "<html> </html>")
       (test-collapse-whitespace #t "<html><![CDATA[   ]]></html>" "<html><![CDATA[   ]]></html>")
       (test-collapse-whitespace #t "<html><![CDATA[\n]]></html>" "<html><![CDATA[\n]]></html>")
       (test-collapse-whitespace #t "<html><![CDATA[\t]]></html>" "<html><![CDATA[\t]]></html>")
       (test-collapse-whitespace #f "<html>\n</html>" "<html>\n</html>"))
      
      (test-suite
       "read-comments"
       (test-read-comments #f "<html><!-- Foo --></html>" "<html />")
       (test-read-comments #t "<html><!-- Foo --></html>" "<html><!-- Foo --></html>"))
      
      (local
        [(define (test-xexpr-drop-empty-attributes v istr xe)
           (test-equal? (format "~S" (list v istr))
                        (parameterize ([xexpr-drop-empty-attributes v])
                          (xml->xexpr (document-element (read-xml (open-input-string istr)))))
                        xe))]
        (test-suite
         "xexpr-drop-empty-attributes"
         
         (test-xexpr-drop-empty-attributes #f "<html />" '(html ()))
         (test-xexpr-drop-empty-attributes #t "<html />" '(html))
         (test-xexpr-drop-empty-attributes #f "<html>Hey</html>" '(html () "Hey"))
         (test-xexpr-drop-empty-attributes #t "<html>Hey</html>" '(html "Hey"))
         (test-xexpr-drop-empty-attributes #f "<a href=\"#\">Hey</a>" '(a ([href "#"]) "Hey"))
         (test-xexpr-drop-empty-attributes #t "<a href=\"#\">Hey</a>" '(a ([href "#"]) "Hey"))))))
   
   (local 
     [(define example
        `(dict (assoc-pair "first-key"
                           "just a string with some  whitespace in it")
               (assoc-pair "second-key"
                           (false))
               (assoc-pair "third-key"
                           (dict ))
               (assoc-pair "fourth-key"
                           (dict (assoc-pair "inner-key"
                                             (real 3.432))))
               (assoc-pair "fifth-key"
                           (array (integer 14)
                                  "another string"
                                  (true)))
               (assoc-pair "sixth-key"
                           (array))
               (assoc-pair "seventh-key"
                           (data "some data"))
               (assoc-pair "eighth-key"
                           (date "2013-05-10T20:29:55Z"))))
      (define example-str #<<END
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist SYSTEM "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="0.9"><dict><key>first-key</key><string>just a string with some  whitespace in it</string><key>second-key</key><false /><key>third-key</key><dict /><key>fourth-key</key><dict><key>inner-key</key><real>3.432</real></dict><key>fifth-key</key><array><integer>14</integer><string>another string</string><true /></array><key>sixth-key</key><array /><key>seventh-key</key><data>some data</data><key>eighth-key</key><date>2013-05-10T20:29:55Z</date></dict></plist>
END
        )]
     (test-suite
      "PList Library"
      
      (test-not-false 
       "plist-dict?"
       (plist-dict? 
        example))
      (test-false
       "plist-dict?"
       (plist-dict?
        `(p "Hey")))
      (test-false
       "plist-dict?"
       (plist-dict?
        `(dict (p "Hey"))))
      (test-false
       "plist-dict?"
       (plist-dict?
        `(dict (assoc-pair "key" 2 3))))
      (test-false
       "plist-dict?"
       (plist-dict?
        `(dict (assoc-pair 1 2))))
      (test-false
       "plist-dict?"
       (plist-dict?
        `(dict (assoc-pair "key" #f))))
      
      (test-equal? "read-plist"
                   (read-plist (open-input-string example-str))
                   example)
      
      (test-equal? "write-plist"
                   (with-output-to-string
                       (lambda ()
                         (write-plist example (current-output-port))))
                   example-str)
      
      (local [(define (test-plist-round-trip plist)
                (define-values (in out) (make-pipe))
                (write-plist plist out)
                (close-output-port out)
                (test-equal? (format "~S" plist) (read-plist in) plist))]
        (test-plist-round-trip example)

        (let ()
          (define array '(array "apple" 
                                (true)
                                (false)
                                (integer 5)
                                (real 5.0)))
          (test-not-false
           "plist-value?"
           (plist-value? array))
          (test-plist-round-trip array)
          (for ([v (in-list (cdr array))])
            (test-not-false
             "plist-value?"
             (plist-value? v))
            (test-plist-round-trip v))))))))

(module+ test
  (run-tests xml-tests))
