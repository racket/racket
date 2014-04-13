#lang racket/base

(require racket/contract
         racket/class
         racket/gui/base
         "private/dot.rkt" 
         "private/graph.rkt")

(provide
 (contract-out
  [dot-positioning (->* ((is-a?/c pasteboard%)) 
                        ((or/c dot-label neato-label neato-hier-label neato-ipsep-label)
                         boolean?)
                        void?)]
  [find-dot (->* () (boolean?) (or/c path? #f))]))

(provide dot-label neato-label neato-hier-label neato-ipsep-label)

(provide graph-snip<%>
         graph-snip-mixin
         graph-pasteboard<%>
         graph-pasteboard-mixin)

(provide
 (contract-out
  [add-links
   (case->
    (-> (is-a?/c graph-snip<%>) (is-a?/c graph-snip<%>) void?)
    (-> (is-a?/c graph-snip<%>)
        (is-a?/c graph-snip<%>)
        (or/c #f (is-a?/c pen%))
        (or/c #f (is-a?/c pen%))
        (or/c #f (is-a?/c brush%))
        (or/c #f (is-a?/c brush%))
        void?)
    (-> (is-a?/c graph-snip<%>)
        (is-a?/c graph-snip<%>)
        (or/c #f (is-a?/c pen%))
        (or/c #f (is-a?/c pen%))
        (or/c #f (is-a?/c brush%))
        (or/c #f (is-a?/c brush%))
        (or/c #f string?)
        void?)
    (-> (is-a?/c graph-snip<%>)
        (is-a?/c graph-snip<%>)
        (or/c #f (is-a?/c pen%))
        (or/c #f (is-a?/c pen%))
        (or/c #f (is-a?/c brush%))
        (or/c #f (is-a?/c brush%))
        number?
        number?
        void?)
    (-> (is-a?/c graph-snip<%>)
        (is-a?/c graph-snip<%>)
        (or/c #f (is-a?/c pen%))
        (or/c #f (is-a?/c pen%))
        (or/c #f (is-a?/c brush%))
        (or/c #f (is-a?/c brush%))
        number?
        number?
        (or/c #f string?)
        void?))]
  [add-links/text-colors
   (-> (is-a?/c graph-snip<%>)
       (is-a?/c graph-snip<%>)
       (or/c #f (is-a?/c pen%))
       (or/c #f (is-a?/c pen%))
       (or/c #f (is-a?/c brush%))
       (or/c #f (is-a?/c brush%))
       (or/c #f (is-a?/c color%))
       (or/c #f (is-a?/c color%))
       number?
       number?
       (or/c #f string?)
       void?)]
  [remove-links
   (-> (is-a?/c graph-snip<%>)
       (is-a?/c graph-snip<%>)
       void?)]
  [set-link-label
   (-> (is-a?/c graph-snip<%>)
       (is-a?/c graph-snip<%>)
       (or/c #f string?)
       void?)]))
