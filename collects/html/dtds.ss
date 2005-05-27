;; copyright by Paul Graunke June 2000 AD
(define-signature dtd-ast^
  ((struct dtd-item ())
   (struct element-def (name start-optional stop-optional content))
   (struct att-list (name junk))
   (struct entity-def (name value))
   (struct thingy (uh whatever))))

(define-signature dtd^ (read-sgml-dtd summarize-dtd))
(define-signature entity-expander^ (empty-entity-expander extend-entity-expander expand-entities))

(require-library "functios.ss")
(require-library "strings.ss")
