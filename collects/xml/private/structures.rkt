#lang racket/base
(require racket/contract)

; Location = (make-location Nat Nat Nat) | Symbol
(define-struct location (line char offset) #:transparent)

; Source = (make-source Location Location)
(define-struct source (start stop) #:transparent)

; Document = (make-document Prolog Element (listof Misc))
(define-struct document (prolog element misc) #:transparent)

; Prolog = (make-prolog (listof Misc) Document-type (listof Misc))
(define-struct prolog (misc dtd misc2) #:transparent)

; Document-type = (make-document-type sym External-dtd #f)
;               | #f
(define-struct document-type (name external inlined) #:transparent)

; External-dtd = (make-external-dtd/public str str)
;              | (make-external-dtd/system str)
;              | #f
(define-struct external-dtd (system) #:transparent)
(define-struct (external-dtd/public external-dtd) (public) #:transparent)
(define-struct (external-dtd/system external-dtd) () #:transparent)

; Element = (make-element Location Location Symbol (listof Attribute) (listof Content))
(define-struct (element source) (name attributes content) #:transparent)

; Attribute = (make-attribute Location Location Symbol String)
(define-struct (attribute source) (name value) #:transparent)

; Pcdata = (make-pcdata Location Location String)
(define-struct (pcdata source) (string) #:transparent)

; Cdata = (make-cdata Location Location String)
(define-struct (cdata source) (string) #:transparent)

; Content = Pcdata  
;         |  Element
;         |  Entity
;         |  Misc
;         |  Cdata

; Misc = Comment
;      |  Processing-instruction

; Section 2.2 of XML 1.1
; (XML 1.0 is slightly different and looks less restrictive)
(define (valid-char? i)
  (and (exact-nonnegative-integer? i)
       (or (= i #x9)
           (= i #xA)
           (= i #xD)
           (<= #x20 i #xD7FF)
           (<= #xE000 i #xFFFD)
           (<= #x10000 i #x10FFFF))))

; Entity = (make-entity Location Location (U Nat Symbol))
(define-struct (entity source) (text) #:transparent)

; Processing-instruction = (make-p-i Location Location String String)
; also represents XMLDecl
(define-struct (p-i source) (target-name instruction) #:transparent)

; Comment = (make-comment String)
(define-struct comment (text) #:transparent)

; permissive-xexprs : parameter bool
(define permissive-xexprs (make-parameter #f))

(define permissive/c
  (make-contract
   #:name 'permissive/c
   #:projection
   (lambda (blame)
     (lambda (v)
       (if (permissive-xexprs)
         v
         (raise-blame-error
          blame v "not in permissive mode"))))
   #:first-order
   (lambda (v)
     (permissive-xexprs))))

; content? : TST -> Bool
(define content/c
  (or/c pcdata? element? entity? comment? cdata? p-i? permissive/c))

(define misc/c
  (or/c comment? p-i?))

(define location/c
  (or/c location? symbol? false/c))
(provide/contract
 (struct location ([line exact-nonnegative-integer?]
                   [char exact-nonnegative-integer?]
                   [offset exact-nonnegative-integer?]))
 [location/c contract?]
 (struct source ([start location/c]
                 [stop location/c]))
 (struct external-dtd ([system string?]))
 (struct (external-dtd/public external-dtd) ([system string?]
                                             [public string?]))
 (struct (external-dtd/system external-dtd) ([system string?]))
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
