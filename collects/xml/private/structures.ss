#lang scheme
(require "sig.ss")

(provide xml-structs@)

(define-unit xml-structs@
  (import)
  (export xml-structs^)
  
  ; Location = (make-location Nat Nat Nat) | Symbol
  (define-struct location (line char offset))
  
  ; Source = (make-source Location Location)
  (define-struct source (start stop))
  
  ; Document = (make-document Prolog Element (listof Misc))
  (define-struct document (prolog element misc))
  
  ; Prolog = (make-prolog (listof Misc) Document-type (listof Misc))
  (define-struct prolog (misc dtd misc2))
  
  ; Document-type = (make-document-type sym External-dtd #f)
  ;               | #f
  (define-struct document-type (name external inlined))
  
  ; External-dtd = (make-external-dtd/public str str)
  ;              | (make-external-dtd/system str)
  ;              | #f
  (define-struct external-dtd (system))
  (define-struct (external-dtd/public external-dtd) (public))
  (define-struct (external-dtd/system external-dtd) ())
  
  ; Element = (make-element Location Location Symbol (listof Attribute) (listof Content))
  (define-struct (element source) (name attributes content))
  
  ; Attribute = (make-attribute Location Location Symbol String)
  (define-struct (attribute source) (name value))
  
  ; Pcdata = (make-pcdata Location Location String)
  (define-struct (pcdata source) (string))
  
  ; Cdata = (make-cdata Location Location String)
  (define-struct (cdata source) (string))
  
  ; Content = Pcdata  
  ;         |  Element
  ;         |  Entity
  ;         |  Misc
  ;         |  Cdata
  
  ; Misc = Comment
  ;      |  Processing-instruction
  
  ; Entity = (make-entity Location Location (U Nat Symbol))
  (define-struct (entity source) (text))
  
  ; Processing-instruction = (make-p-i Location Location String String)
  ; also represents XMLDecl
  (define-struct (p-i source) (target-name instruction))
  
  ; Comment = (make-comment String)
  (define-struct comment (text))
  
  ; content? : TST -> Bool
  (define (content? x)
    (or (pcdata? x) 
        (element? x)
        (entity? x)
        (comment? x)
        (cdata? x)
        (p-i? x))))
