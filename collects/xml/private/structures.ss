
(module structures mzscheme
  (require (lib "unitsig.ss"))

  (require "sig.ss")

  (provide xml-structs@)

  (define xml-structs@
    (unit/sig xml-structs^
      (import)
      
      ; Location = (make-location Nat Nat Nat) | Symbol
      (define-struct location (line char offset))

      ; Source = (make-source Location Location)
      (define-struct source (start stop))
      
      ; Document = (make-document Prolog Element (listof Misc))
      (define-struct document (prolog element misc))
      
      ; Prolog = (make-prolog (listof Misc) Document-type [Misc ...])
      ; The Misc items after the Document-type are optional arguments to maintain
      ; backward compatability with older versions of the XML library.
      ;(define-struct prolog (misc dtd misc2))

      (define-values (struct:prolog real-make-prolog prolog? access-prolog set-prolog!)
        (make-struct-type 'prolog #f 3 0))
      
      (define (make-prolog misc dtd . misc2)
        (real-make-prolog misc dtd misc2))
      
      (define prolog-misc (make-struct-field-accessor access-prolog 0 'misc))
      (define set-prolog-misc! (make-struct-field-mutator set-prolog! 0 'misc))
      
      (define prolog-dtd (make-struct-field-accessor access-prolog 1 'dtd))
      (define set-prolog-dtd! (make-struct-field-mutator set-prolog! 1 'dtd))
      
      (define prolog-misc2 (make-struct-field-accessor access-prolog 2 'misc2))
      (define set-prolog-misc2! (make-struct-field-mutator set-prolog! 2 'misc2))
      
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
      
      ; Content = Pcdata  
      ;         |  Element
      ;         |  Entity
      ;         |  Misc
      
      ; Misc = Comment
      ;      |  Processing-instruction
      
      ; Entity = (make-entity Location Location (U Nat Symbol))
      (define-struct (entity source) (text))
      
      ; Processing-instruction = (make-pi Location Location String (list String))
      ; also represents XMLDecl
      (define-struct (pi source) (target-name instruction))
      
      ; Comment = (make-comment String)
      (define-struct comment (text))
      
      ; content? : TST -> Bool
      (define (content? x)
	(or (pcdata? x) (element? x) (entity? x) (comment? x) (pi? x))))))
