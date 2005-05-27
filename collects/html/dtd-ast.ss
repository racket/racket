;; copyright by Paul Graunke June 2000 AD
(module dtd-ast mzscheme
  (provide (struct dtd-item ())
           (struct element-def (name start-optional stop-optional content))
           (struct att-list (name junk))
           (struct entity-def (name value))
           (struct thingy (uh whatever)))
  
  
  ;; Dtd ::= (listof Dtd-item)
  ;; Dtd-item ::= (make-element-def (listof Symbol) Bool Bool Content-model)
  ;;           |  (make-att-list (listof Symbol) String) ;; more here - parse the String
  ;;           |  (make-entity-def Symbol String)
  ;;           |  (make-thingy String String) ;; more here - what is <![ foo [ bar baz...]]> this?
  (define-struct dtd-item ())
  (define-struct (element-def struct:dtd-item) (name start-optional stop-optional content))
  (define-struct (att-list struct:dtd-item) (name junk))
  (define-struct (entity-def struct:dtd-item) (name value))
  (define-struct (thingy struct:dtd-item) (uh whatever))
  
  ;; Content-model ::= String
  ;; more here - parse content-models
  )
