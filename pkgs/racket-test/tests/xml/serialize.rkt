#lang racket/base

(module+ test
  (require xml
           racket/serialize
           rackunit)

  (define doc
    (document
     (prolog (list (comment "Opening comment")
                   (p-i (location 2 0 24)
                        (location 2 21 45)
                        'xml
                        "version=\"1.0\""))
             #f
             (list (p-i #f #f 'xml-stylesheet "type=\"text/css\" href=\"style.css\"")))
     (element (location #f #f 98)
              'source-stop/symbolic
              'doc
              (list (attribute #f #f 'color "yellow")
                    (attribute #f #f 'shape "circle"))
              (list (pcdata #f #f "Programming languages should be designed ")
                    (element #f #f 'i '() (list (p-i #f #f 'bgcolor "#ffe303") (pcdata #f #f "not")))
                    (pcdata #f #f "by piling feature on top of feature")
                    (entity #f #f (char->integer #\,))
                    (comment "encoded ,")
                    (pcdata #f #f " but by removing the weaknesses ")
                    (entity #f #f 'amp)
                    (cdata #f #f "<![CDATA[ ]]>")
                    (pcdata #f #f "restrictions that make additional features appear necessary.")))
     (list (p-i #f #f 'custom "")
           (comment "Here endeth the lesson."))))

  (define (doc-of-type type)
    (document
     (prolog '() type '())
     (element #f #f (document-type-name type) '() '())
     '()))

  (define svg
    (doc-of-type
     (document-type 'svg
                    (external-dtd/public "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"
                                         "-//W3C//DTD SVG 1.1//EN")
                    #f)))

  (define system
    (doc-of-type (document-type 'system (external-dtd/system "system.dtd") #f)))

  (define html
    (doc-of-type (document-type 'html (external-dtd "ignored") #f)))
  #;(begin
      html (write-xml html) (newline)
      svg (write-xml svg) (newline)
      system (write-xml system))

  (define-binary-check (check-equal-always? equal-always? actual expected))

  (check-equal-always?
   (deserialize (serialize doc))
   doc
   "deserialization should be the inverse of serialization: doc")

  (check-equal-always?
   (deserialize (serialize svg))
   svg
   "deserialization should be the inverse of serialization: svg")

  (check-equal-always?
   (deserialize (serialize system))
   system
   "deserialization should be the inverse of serialization: system")

  (check-equal-always?
   (deserialize (serialize html))
   html
   "deserialization should be the inverse of serialization: html")

  (test-exn
   "deserialize enforces contracts: document"
   #rx"make-document: contract violation.*given: 'bad.*  blaming: (.* deserialize-info)"
   (λ ()
     (deserialize
      '((3)
        3
        (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:document-v0)
         ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:prolog-v0)
         ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:element-v0))
        0 () ()
        (0 (1 () #f ()) (2 #f #f a () ()) bad)))))

  (test-exn
   "deserialize enforces contracts: prolog"
   #rx"make-prolog: contract violation.*given: 'bad.*  blaming: (.* deserialize-info)"
   (λ ()
     (deserialize
      '((3)
        1
        (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:prolog-v0))
        0 () ()
        (0 () bad ())))))

  (test-exn
   "deserialize enforces contracts: document-type"
   #rx"make-document-type: contract violation.*given: 'bad.*  blaming: (.* deserialize-info)"
   (λ ()
     (deserialize
      '((3)
        2
        (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:document-type-v0)
         ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:external-dtd-v0))
        0 () ()
        (0 x (1 "") bad)))))

  (test-exn
   "deserialize enforces contracts: external-dtd"
   #rx"make-external-dtd: contract violation.*given: 'bad.*  blaming: (.* deserialize-info)"
   (λ ()
     (deserialize
      ;; this test also makes sure nesting works
      '((3)
        2
        (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:document-type-v0)
         ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:external-dtd-v0))
        0 () ()
        (0 x (1 bad) #f)))))

  (test-exn
   "deserialize enforces contracts: external-dtd/system"
   #rx"make-external-dtd/system: contract violation.*given: 'bad.*  blaming: (.* deserialize-info)"
   (λ ()
     (deserialize
      '((3)
        1
        (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:external-dtd/system-v0))
        0 () ()
        (0 bad)))))

  (test-exn
   "deserialize enforces contracts: external-dtd/public"
   #rx"make-external-dtd/public: contract violation.*given: 'bad.*  blaming: (.* deserialize-info)"
   (λ ()
     (deserialize
      '((3)
        1
        (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:external-dtd/public-v0))
        0 () ()
        (0  "x.dtd" bad)))))

  (test-exn
   "deserialize enforces contracts: comment"
   #rx"make-comment: contract violation.*given: 'bad.*  blaming: top-level"
   (λ ()
     (deserialize
      '((3)
        1
        (((submod (lib "xml/private/core.rkt") deserialize-info) . deserialize-info:comment-v0))
        0 () ()
        (0 bad)))))

  (test-exn
   "deserialize enforces contracts: p-i"
   #rx"make-p-i: contract violation.*given: 'bad.*  blaming: top-level"
   (λ ()
     (deserialize
      '((3)
        1
        (((submod (lib "xml/private/core.rkt") deserialize-info) . deserialize-info:p-i-v0))
        0 () ()
        (0 #f #f xml bad)))))

  (test-exn
   "deserialize enforces contracts: location"
   #rx"make-location: contract violation.*given: 'bad.*  blaming: (.* deserialize-info)"
   (λ ()
     (deserialize
      '((3)
        1
        (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:location-v0))
        0 ()()
        (0 #f #f bad)))))

  (test-exn
   "deserialize enforces contracts: element"
   #rx"make-element: contract violation.*given: 'bad.*  blaming: (.* deserialize-info)"
   (λ ()
     (deserialize
      '((3)
        1
        (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:element-v0))
        0 () ()
        (0 #f #f i bad ())))))

  (test-exn
   "deserialize enforces contracts: attribute"
   #rx"make-attribute: contract violation.*given: 'bad.*  blaming: (.* deserialize-info)"
   (λ ()
     (deserialize
      '((3)
        1
        (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:attribute-v0))
        0 () ()
        (0 #f #f id bad)))))

  (test-exn
   "deserialize enforces contracts: pcdata"
   #rx"make-pcdata: contract violation.*given: 'bad.*  blaming: (.* deserialize-info)"
   (λ ()
     (deserialize
      '((3)
        1
        (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:pcdata-v0))
        0 () ()
        (0 #f #f bad)))))

  (test-exn
   "deserialize enforces contracts: entity"
   #rx"make-entity: contract violation.*given: #\"bad\".*  blaming: (.* deserialize-info)"
   (λ ()
     (deserialize
      '((3)
        1
        (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:entity-v0))
        0 () ()
        (0 #f #f #"bad")))))

  (test-exn
   "deserialize enforces contracts: cdata"
   #rx"make-cdata: contract violation.*given: 'bad.*  blaming: top-level"
   (λ ()
     (deserialize
      '((3)
        1
        (((submod (lib "xml/private/core.rkt") deserialize-info) . deserialize-info:cdata-v0))
        0 () ()
        (0 #f #f bad)))))

  (define serialized-doc
    '((3)
      10
      (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:document-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:prolog-v0)
       ((submod (lib "xml/private/core.rkt") deserialize-info) . deserialize-info:comment-v0)
       ((submod (lib "xml/private/core.rkt") deserialize-info) . deserialize-info:p-i-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:location-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:element-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:attribute-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:pcdata-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:entity-v0)
       ((submod (lib "xml/private/core.rkt") deserialize-info) . deserialize-info:cdata-v0))
      0 () ()
      (0
       (1
        (c (2 "Opening comment") c (3 (4 2 0 24) (4 2 21 45) xml "version=\"1.0\""))
        #f
        (c (3 #f #f xml-stylesheet "type=\"text/css\" href=\"style.css\"")))
       (5
        (4 #f #f 98)
        source-stop/symbolic
        doc
        (c (6 #f #f color "yellow") c (6 #f #f shape "circle"))
        (c
         (7 #f #f "Programming languages should be designed ")
         c
         (5 #f #f i () (c (3 #f #f bgcolor "#ffe303") c (7 #f #f "not")))
         c
         (7 #f #f "by piling feature on top of feature")
         c
         (8 #f #f 44)
         c
         (2 "encoded ,")
         c
         (7 #f #f " but by removing the weaknesses ")
         c
         (8 #f #f amp)
         c
         (9 #f #f "<![CDATA[ ]]>")
         c
         (7 #f #f "restrictions that make additional features appear necessary.")))
       (c (3 #f #f custom "") c (2 "Here endeth the lesson.")))))

  (define serialized-svg
    '((3)
      5
      (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:document-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:prolog-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:document-type-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:external-dtd/public-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:element-v0))
      0 () ()
      (0
       (1 () (2 svg (3 "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd" "-//W3C//DTD SVG 1.1//EN") #f) ())
       (4 #f #f svg () ())
       ())))

  (define serialized-system
    '((3)
      5
      (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:document-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:prolog-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:document-type-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:external-dtd/system-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:element-v0))
      0 () ()
      (0 (1 () (2 system (3 "system.dtd") #f) ()) (4 #f #f system () ()) ())))

  (define serialized-html
    '((3)
      5
      (((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:document-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:prolog-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:document-type-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:external-dtd-v0)
       ((submod (lib "xml/private/structures.rkt") deserialize-info) . deserialize-info:element-v0))
      0 () ()
      (0 (1 () (2 html (3 "ignored") #f) ()) (4 #f #f html () ()) ())))

  (test-case
   "compatibility of old serialized output: doc"
   (check-not-exn (λ ()
                    (deserialize serialized-doc))
                  "should deserialize without errors")
   (check-equal-always? (deserialize serialized-doc)
                        doc
                        "check result from deserialize"))

  (test-case
   "compatibility of old serialized output: svg"
   (check-not-exn (λ ()
                    (deserialize serialized-svg))
                  "should deserialize without errors")
   (check-equal-always? (deserialize serialized-svg)
                        svg
                        "check result from deserialize"))

  (test-case
   "compatibility of old serialized output: system"
   (check-not-exn (λ ()
                    (deserialize serialized-system))
                  "should deserialize without errors")
   (check-equal-always? (deserialize serialized-system)
                        system
                        "check result from deserialize"))

  (test-case
   "compatibility of old serialized output: html"
   (check-not-exn (λ ()
                    (deserialize serialized-html))
                  "should deserialize without errors")
   (check-equal-always? (deserialize serialized-html)
                        html
                        "check result from deserialize")))
