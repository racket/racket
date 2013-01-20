#lang racket
(require rackunit
         rackunit/text-ui
         net/url
         (prefix-in h: html)
         (prefix-in x: xml))

(define html-tests
  (test-suite
   "HTML"

   (test-case
    "Example"
    (local
     [(define an-html
        (h:read-xhtml
         (open-input-string
          (string-append
           "<html><head><title>My title</title></head><body>"
           "<p>Hello world</p><p><b>Testing</b>!</p>"
           "</body></html>"))))

                                        ; extract-pcdata: html-content -> (listof string)
                                        ; Pulls out the pcdata strings from some-content.
      (define (extract-pcdata some-content)
        (cond [(x:pcdata? some-content)
               (list (x:pcdata-string some-content))]
              [(x:entity? some-content)
               (list)]
              [else
               (extract-pcdata-from-element some-content)]))

                                        ; extract-pcdata-from-element: html-element -> (listof string)
                                        ; Pulls out the pcdata strings from an-html-element.
      (define (extract-pcdata-from-element an-html-element)
        (match an-html-element
          [(struct h:html-full (attributes content))
           (apply append (map extract-pcdata content))]

          [(struct h:html-element (attributes))
           '()]))]

     (check-equal? (extract-pcdata an-html)
                   ' ("My title" "Hello world" "Testing" "!"))))


   (test-case
    "Eli - March 1"
    (check-not-false
     (lambda ()
       (h:read-html-as-xml
        (get-pure-port
         (string->url
          "http://list.cs.brown.edu/pipermail/plt-scheme/"))))))

   (test-case
    "DW - Jan 18 2013"
    (check-equal?
     (h:read-html-as-xml (open-input-string "<новости новости=\"новости\">"))
     (list
      (x:element
       (x:location 0 0 0)
       (x:location 0 0 48)
       'новости
       (list (x:attribute (x:location 0 0 16) (x:location 0 0 47) 'новости "новости"))
       '()))))))

(run-tests html-tests)
