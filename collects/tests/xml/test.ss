#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         xml
         xml/plist
         "to-list.ss")

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
(define (test-read-xml str xml)
  (test-equal? str (document->list (read-xml (open-input-string str))) xml))

(define test-syntax:read-xml/exn (mk-test-read-xml/exn syntax:read-xml))
(define (test-syntax:read-xml str xml)
  (test-equal? str (syntax->datum (syntax:read-xml (open-input-string str))) xml))

(define test-read-xml/element/exn (mk-test-read-xml/exn read-xml/element))
(define (test-read-xml/element str xml)
  (test-equal? str (element->list (read-xml/element (open-input-string str))) xml))

(define test-syntax:read-xml/element/exn (mk-test-read-xml/exn syntax:read-xml/element))
(define (test-syntax:read-xml/element str xml)
  (test-equal? str (syntax->datum (read-xml/element (open-input-string str))) xml))

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

(define xml-tests
  (test-suite
   "XML"
   
   (test-suite
    "Datatypes"
    (test-suite 
     "xexpr"
     (test-xexpr? "string")
     (test-xexpr? (list 'a (list (list 'href "#")) "content"))
     (test-xexpr? (list 'p "one" "two" "three"))
     (test-xexpr? 'nbsp)
     (test-xexpr? 10)
     (test-xexpr? (make-cdata #f #f "unquoted <b>"))
     (test-xexpr? (make-comment "Comment!"))
     (test-xexpr? (make-pcdata #f #f "quoted <b>"))
     
     (test-not-xexpr? +)
     (test-not-xexpr? #f))
    
    (test-not-false "xexpr/c" (contract? xexpr/c))
    
    (test-not-false "document" (document? (make-document (make-prolog empty #f) (make-element #f #f 'br empty empty) empty)))
    
    (test-not-false "prolog" (prolog? (make-prolog empty #f)))
    (let ([c1 (make-comment "c1")]
          [c2 (make-comment "c2")])
      (test-equal? "prolog" (prolog-misc2 (make-prolog empty #f c1 c2))
                   (list c1 c2)))
    
    (test-not-false "document-type" (document-type? (make-document-type 'name (make-external-dtd "string") #f)))
    
    (test-not-false "external-dtd" (external-dtd? (make-external-dtd "string")))
    (test-not-false "external-dtd/public" (external-dtd/public? (make-external-dtd/public "string" "public")))
    (test-not-false "external-dtd/system" (external-dtd/system? (make-external-dtd/system "string")))
    
    (test-not-false "element" (element? (make-element #f #f 'br empty empty)))
    
    (test-not-false "content? pcdata" (content? (make-pcdata #f #f "pcdata")))
    (test-not-false "content? element" (content? (make-element #f #f 'br empty empty)))
    (test-not-false "content? entity" (content? (make-entity #f #f 'nbsp)))
    (test-not-false "content? comment" (content? (make-comment "string")))
    (test-not-false "content? cdata" (content? (make-cdata #f #f "cdata")))
    
    (test-not-false "attribute" (attribute? (make-attribute #f #f 'name "value")))
    
    (test-not-false "entity symbol" (entity? (make-entity #f #f 'nbsp)))
    (test-not-false "entity number" (entity? (make-entity #f #f 10)))
    
    (test-not-false "pcdata" (pcdata? (make-pcdata #f #f "string")))
    
    (test-not-false "cdata" (cdata? (make-cdata #f #f "string")))
    
    (test-not-false "p-i" (p-i? (make-p-i #f #f "target" "instruction")))
    
    (test-not-false "comment" (comment? (make-comment "text")))
    
    (test-not-false "source" (source? (make-source 'start 'stop)))
    (test-not-false "source" (source? (make-source (make-location 1 2 3) 'stop)))
    (test-not-false "source" (source? (make-source 'start (make-location 1 2 3))))
    (test-not-false "source" (source? (make-source (make-location 1 2 3) (make-location 4 5 6))))
    
    (test-not-false "exn:invalid-xexpr" (exn:invalid-xexpr? (make-exn:invalid-xexpr "string" (current-continuation-marks) 'nbsp))))
   
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
     (test-read-xml/exn "<br /><br />" "read-xml: parse-error: extra stuff at end of document #<element>")
     
     (test-read-xml 
      "<doc><bold>hi</bold> there!</doc>"
      '(make-document
        (make-prolog (list) #f)
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
        (make-prolog (list) #f)
        (make-element
         (make-source (make-location 1 0 1) (make-location 1 21 22))
         'a
         (list (make-attribute (make-source (make-location 1 3 4) (make-location 1 11 12)) href "#"))
         (list (make-pcdata (make-source (make-location 1 12 13) (make-location 1 17 18)) "inner")))
        (list)))
     
     (test-read-xml
      "<root>&nbsp;</root>"
      '(make-document
        (make-prolog (list) #f)
        (make-element
         (make-source (make-location 1 0 1) (make-location 1 19 20))
         'root
         (list)
         (list (make-entity (make-source (make-location 1 6 7) (make-location 1 12 13)) 'nbsp)))
        (list)))
     
     (test-read-xml
      "<root>&#40;</root>"
      '(make-document
        (make-prolog (list) #f)
        (make-element
         (make-source (make-location 1 0 1) (make-location 1 18 19))
         'root
         (list)
         (list (make-entity (make-source (make-location 1 6 7) (make-location 1 11 12)) '40)))
        (list)))
     
     (test-read-xml
      "<!-- comment --><br />"
      '(make-document
        (make-prolog (list) #f)
        (make-element (make-source (make-location 1 16 17) (make-location 1 22 23)) 'br (list) (list))
        (list)))
     
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
      "read-xml: parse-error: expected root element - received #<comment>")
     
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
     (test-syntax:read-xml/exn "<br /><br />" "read-xml: parse-error: extra stuff at end of document #<element>")
     
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
     
     (test-syntax:read-xml/exn
      "<!-- comment --><br />"
      "read-xml: parse-error: expected root element - received #f")
     
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
      "read-xml: parse-error: expected root element - received #f")
     
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
   
   (test-suite 
    "XML and X-expression Conversions"
    
    ; XXX permissive?
    
    ; XXX xml->xexpr
    
    ; XXX xexpr->string
    
    ; XXX eliminate-whitespace
    
    ; XXX validate-xexpr
    
    ; XXX correct-xexpr?
    
    )
   
   (test-suite
    "Parameters"
    
    ; XXX empty-tag-shorthand
    
    ; XXX html-empty-tags
    
    ; XXX collapse-whitespace
    
    ; XXX read-comments
    
    ; XXX xexpr-drop-empty-attributes
    
    )
   
   (local [(define example
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
                                (array))))
           (define example-str #<<END
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist SYSTEM "file://localhost/System/Library/DTDs/PropertyList.dtd">
<plist version="0.9"><dict><key>first-key</key><string>just a string with some  whitespace in it</string><key>second-key</key><false /><key>third-key</key><dict /><key>fourth-key</key><dict><key>inner-key</key><real>3.432</real></dict><key>fifth-key</key><array><integer>14</integer><string>another string</string><true /></array><key>sixth-key</key><array /></dict></plist>
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
        (test-plist-round-trip example))
      
      ))
   
   (test-suite
    "Legacy tests"   
    
    (test-suite
     "xml->xexpr"
     (test-exn
      "Non-permissive"
      (lambda (exn)
        (and (exn? exn)
             (regexp-match #rx"Expected content," (exn-message exn))))
      (lambda ()
        (xml->xexpr #f)))
     
     (test-false
      "Permissive"
      (parameterize ([permissive? #t])
        (xml->xexpr #f))))
    
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
    
    (let ()
      (define a-pi (make-p-i #f #f "foo" "bar"))
      (define a-p (make-prolog empty #f))
      (define a-p/pi (make-prolog (list a-pi) #f))
      (define a-d0
        (make-document a-p (make-element #f #f 'html empty empty)
                       empty))
      (define a-d1
        (make-document a-p (make-element #f #f 'html empty empty)
                       (list a-pi)))
      (define a-d2
        (make-document a-p/pi (make-element #f #f 'html empty empty)
                       (list a-pi)))
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
                    "<?foo bar?>\n\n<html /><?foo bar?>\n"))))))

(run-tests xml-tests)