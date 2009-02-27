#lang scheme

(define-signature xml-structs^
  ((struct/ctc location ([line exact-nonnegative-integer?]
                         [char exact-nonnegative-integer?]
                         [offset exact-nonnegative-integer?]))
   (struct/ctc source ([start (or/c location? symbol?)]
                       [stop (or/c location? symbol?)]))
   (struct/ctc comment ([text string?]))
   (struct pcdata (string)) ; XXX needs parent
   (struct cdata (string)) ; XXX needs parent
   (struct/ctc document-type ([name symbol?]
                              #;[external external-dtd?]
                              ; XXX results in this error
                              ;     ->: expected contract or a value that can be coerced into one, got #<undefined>
                              ;     I presume that there is a letrec somewhere
                              [external any/c]
                              [inlined false/c]))
   (struct/ctc document (#;[prolog prolog?] ; XXX same as above
                         [prolog any/c]
                         #;[element element?]
                         [element any/c]
                         #;[misc (listof (or/c comment? pi?))]
                         [misc (listof any/c)]))
   (struct/ctc prolog (#;[misc (listof (or/c comment? pi?))] ; XXX same as above
                       [misc (listof any/c)]
                       #;[dtd document-type?]
                       [dtd any/c]
                       #;[misc2 (listof (or/c comment? pi?))]
                       [misc2 (listof any/c)]))
   (struct/ctc external-dtd ([system string?]))
   (struct external-dtd/public (public)) ; XXX needs parent
   (struct external-dtd/system ()) ; XXX needs parent
   (struct element (name attributes content)) ; XXX needs parent
   (struct attribute (name value)) ; XXX needs parent
   (struct p-i (target-name instruction)) ; XXX needs parent
   (struct entity (text)) ; XXX needs parent
   (contracted
    [content? (any/c . -> . boolean?)])))

(define-signature writer^
  ((contracted
    [write-xml ((any/c) (output-port?) . ->* . void?)]
    [display-xml ((any/c) (output-port?) . ->* . void?)]
    [write-xml/content ((any/c) (output-port?) . ->* . void?)]
    [display-xml/content ((any/c) (output-port?) . ->* . void?)])
   ; XXX I can't contract the above (well), because they refer to structs from xml-structs^
   (contracted
    [empty-tag-shorthand (parameter/c (or/c (symbols 'always 'never) (listof symbol?)))]
    [html-empty-tags (listof symbol?)])))

(define-signature reader^ 
  ((contracted
    [read-xml (() (input-port?) . ->* . any/c)]
    [read-xml/element (() (input-port?) . ->* . any/c)]
    [read-comments (parameter/c boolean?)]
    [collapse-whitespace (parameter/c boolean?)])
   ; XXX can't contract the above (well) because they refer to structs
   ; XXX can't contract exn:xml beacuse of parent
   (struct exn:xml ())))

(define-signature xexpr^
  ((struct exn:invalid-xexpr (code)) ; XXX needs parent
   (contracted
    [xexpr/c contract?]
    [xexpr? (any/c . -> . boolean?)]
    [xexpr->string (xexpr/c . -> . string?)]
    [xml->xexpr (any/c . -> . xexpr/c)] ; XXX bad because of struct
    [xexpr->xml (xexpr/c . -> . any/c)] ; XXX bad because of struct
    [xexpr-drop-empty-attributes (parameter/c boolean?)]
    [permissive? (parameter/c boolean?)]
    [validate-xexpr (any/c . -> . (one-of/c #t))]
    [correct-xexpr? (any/c (-> any/c) (exn:invalid-xexpr? . -> . any/c) . -> . any/c)]
    [xexpr-attribute? (any/c . -> . boolean?)]
    [listof? ((any/c . -> . boolean?) any/c . -> . boolean?)]
    [attribute->srep (any/c . -> . xexpr-attribute?)] ; XXX bad because of struct
    [bcompose ((any/c any/c . -> . any/c) (any/c . -> . any/c) . -> . (any/c any/c . -> . any/c))]
    [assoc-sort ((listof (list/c symbol? string?)) . -> . (listof (list/c symbol? string?)))])))

(define-signature space^ 
  ((contracted
    ; XXX bad because of struct
    [eliminate-whitespace ((listof symbol?) (boolean? . -> . boolean?) . -> . (any/c . -> . any/c))])))

(provide xml-structs^
         writer^
         reader^
         xexpr^
         space^)
