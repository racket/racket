#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         xml)

;; test-bad-read-input : format-str str -> void
;; First argument is the input, second is the error message
(define (test-read-xml/exn format-str err-string)
  (define str (format format-str))
  (test-exn
   str
   (lambda (x)
     (and (exn:xml? x)
          (equal? (exn-message x) err-string)))
   (lambda ()
     (read-xml (open-input-string str)))))

(define (document->list xml)
  (list 'make-document
        (prolog->list (document-prolog xml))
        (element->list (document-element xml))
        (list* 'list (map misc->list (document-misc xml)))))
(define (prolog->list p)
  (list* 'make-prolog
         (list* 'list (map misc->list (prolog-misc p)))
         (dtd->list (prolog-dtd p))
         (map misc->list (prolog-misc2 p))))
(define (dtd->list d)
  (if d
      (list 'make-document-type
            (document-type-name d)
            (external-dtd->list (document-type-external d))
            (document-type-inlined d))
      #f))
(define (external-dtd->list d)
  (cond
    [(external-dtd/system? d)
     (list 'make-external-dtd/system (external-dtd-system d))]
    [(external-dtd/public? d)
     (list 'make-external-dtd/public (external-dtd-system d) (external-dtd/public-public d))]
    [(external-dtd? d)
     (list 'make-external-dtd (external-dtd-system d))]))
(define (element->list e)
  (list 'make-element
        (source->list e)
        (list 'quote (element-name e))
        (list* 'list (map attribute->list (element-attributes e)))
        (list* 'list (map content->list (element-content e)))))
(define (misc->list e)
  (cond
    [(comment? e)
     (comment->list e)]
    [(p-i? e)
     (p-i->list e)]))
(define (content->list e)
  (cond
    [(pcdata? e) (pcdata->list e)]
    [(element? e) (element->list e)]
    [(entity? e) (entity->list e)]
    [(comment? e) (comment->list e)]
    [(cdata? e) (cdata->list e)]))
(define (attribute->list e)
  (list 'make-attribute
        (source->list e)
        (attribute-name e)
        (attribute-value e)))
(define (entity->list e)
  (list 'make-entity
        (source->list e)
        (list 'quote (entity-text e))))
(define (pcdata->list e)
  (list 'make-pcdata
        (source->list e)
        (pcdata-string e)))
(define (cdata->list e)
  (list 'make-cdata
        (source->list e)
        (cdata-string e)))
(define (p-i->list e)
  (list 'make-p-i
        (source->list e)
        (p-i-target-name e)
        (p-i-instruction e)))
(define (comment->list e)
  (list 'make-comment
        (comment-text e)))
(define (source->list e)
  (list 'make-source
        (location->list (source-start e))
        (location->list (source-stop e))))
(define (location->list e)
  (if (symbol? e)
      e
      (list 'make-location
            (location-line e)
            (location-char e)
            (location-offset e))))


(define (test-read-xml str xml)
  (test-equal? str (document->list (read-xml (open-input-string str))) xml))

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
        (list))))
    
    ; XXX need more

    )
  
  
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
                  "<?foo bar?>\n\n<html /><?foo bar?>\n")))))

(run-tests xml-tests)