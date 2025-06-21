#lang racket/base
(require "core.rkt"
         (submod "core.rkt" serialization-support)
         racket/runtime-path
         racket/contract
         (for-syntax racket/base))

(define-runtime-module-path-index deserialize-info-mpi '(submod "." deserialize-info))
(module+ deserialize-info
  (define-syntax (get-checked-constructor stx)
    (syntax-case stx ()
      [(_ name)
       (identifier? #'name)
       #`(let ()
           (local-require (only-in #,(datum->syntax #'name (list #'submod ".."))
                                   name))
           name)])))
(define-syntax define-xml-struct
  (make-define-serializable-struct #'deserialize-info-mpi #'get-checked-constructor))

; Location = (make-location Nat Nat Nat) | Symbol
(define-xml-struct location (line char offset))

; Document = (make-document Prolog Element (listof Misc))
(define-xml-struct document (prolog element misc))

; Prolog = (make-prolog (listof Misc) Document-type (listof Misc))
(define-xml-struct prolog (misc dtd misc2))

; Document-type = (make-document-type sym External-dtd #f)
;               | #f
(define-xml-struct document-type (name external inlined))

; External-dtd = (make-external-dtd/public str str)
;              | (make-external-dtd/system str)
;              | (external-dtd ignored-str) ; represents *absence* of external dtd
(define-xml-struct external-dtd (system))
(define-xml-struct (external-dtd/public external-dtd) (public))
(define-xml-struct (external-dtd/system external-dtd) ())
(define no-external-dtd
  (external-dtd ""))

; Element = (make-element Location Location Symbol (listof Attribute) (listof Content))
(define-xml-struct (element source) (name attributes content))

; Attribute = (make-attribute Location Location Symbol String)
(define-xml-struct (attribute source) (name value))

; Content = Pcdata
;         |  Element
;         |  Entity
;         |  Misc
;         |  Cdata

; Misc = Comment
;      |  Processing-instruction

; Pcdata = (make-pcdata Location Location String)
(define-xml-struct (pcdata source) (string))

; Entity = (make-entity Location Location (U Nat Symbol))
(define-xml-struct (entity source) (text))

(define permissive/c
  (make-contract
   #:name 'permissive/c
   #:late-neg-projection
   (lambda (blame)
     (lambda (v neg-party)
       (if (permissive-xexprs)
         v
         (raise-blame-error
          blame #:missing-party neg-party
          v '("not in permissive mode" given: "~e") v))))
   #:first-order
   (lambda (v) #t)))

; content? : TST -> Bool
(define content/c
  (or/c pcdata? element? entity? comment? cdata? p-i? permissive/c))

(define misc/c
  (or/c comment? p-i?))

(define location/c
  (or/c location? symbol? false/c))
(provide/contract
 #:unprotected-submodule unsafe
 (struct location ([line (or/c false/c exact-nonnegative-integer?)]
                   [char (or/c false/c exact-nonnegative-integer?)]
                   [offset exact-nonnegative-integer?]))
 [location/c contract?]
 (struct source ([start location/c]
                 [stop location/c]))
 (struct external-dtd ([system string?]))
 (struct (external-dtd/public external-dtd) ([system string?]
                                             [public string?]))
 (struct (external-dtd/system external-dtd) ([system string?]))
 [no-external-dtd (and/c external-dtd?
                         (property/c #:name '|struct type|
                                     (λ (v)
                                       (define-values (t _) (struct-info v))
                                       t)
                                     (flat-named-contract 'struct:external-dtd
                                                          (λ (v)
                                                            (equal? v struct:external-dtd)))))]
 (struct document-type ([name symbol?]
                        [external external-dtd?]
                        [inlined false/c]))
 (struct comment ([text string?]))
 (struct (p-i source) ([start location/c]
                       [stop location/c]
                       [target-name symbol?]
                       [instruction string?]))
 [misc/c contract?]
 (struct prolog ([misc (listof misc/c)]
                 [dtd (or/c document-type? false/c)]
                 [misc2 (listof misc/c)]))
 (struct document ([prolog prolog?]
                   [element element?]
                   [misc (listof misc/c)]))
 (struct (element source) ([start location/c]
                           [stop location/c]
                           [name symbol?]
                           [attributes (listof attribute?)]
                           [content (listof content/c)]))
 (struct (attribute source) ([start location/c]
                             [stop location/c]
                             [name symbol?]
                             [value (or/c string? permissive/c)]))
 [permissive-xexprs (parameter/c boolean?)]
 [permissive/c contract?]
 [content/c contract?]
 (struct (pcdata source) ([start location/c]
                          [stop location/c]
                          [string string?]))
 (struct (cdata source) ([start location/c]
                         [stop location/c]
                         [string string?]))
 [valid-char? (any/c . -> . boolean?)]
 (struct (entity source) ([start location/c]
                          [stop location/c]
                          [text (or/c symbol? valid-char?)])))
