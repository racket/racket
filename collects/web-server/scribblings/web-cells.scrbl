#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "web-cells"]{Web Cells}
@(require (for-label web-server/http
                     web-server/servlet/web-cells
                     web-server/servlet/web))

@defmodule[web-server/servlet/web-cells]{The
@racketmodname[web-server/servlet/web-cells] library provides the
interface to Web cells.

A Web cell is a kind of state defined relative to the @defterm{frame tree}.
The frame-tree is a mirror of the user's browsing session. Every time a
continuation is invoked, a new frame (called the @defterm{current frame}) is
created as a child of the current frame when the continuation was captured.

You should use Web cells if you want an effect to be encapsulated in all
interactions linked from (in a transitive sense) the HTTP response being
generated. For more information on their semantics, consult the paper 
@href-link["http://www.cs.brown.edu/~sk/Publications/Papers/Published/mk-int-safe-state-web/"
"\"Interaction-Safe State for the Web\""].

@defproc[(web-cell? [v any/c])
         boolean?]{
 Determines if @racket[v] is a web-cell.
}

@defproc[(make-web-cell [v any/c])
         web-cell?]{
 Creates a web-cell with a default value of @racket[v].
}

@defproc[(web-cell-ref [wc web-cell?])
         any/c]{
 Looks up the value of @racket[wc] found in the nearest
 frame.
}

@defproc[(web-cell-shadow [wc web-cell?]
                          [v any/c])
         void]{
 Binds @racket[wc] to @racket[v] in the current frame, shadowing any
 other bindings to @racket[wc] in the current frame.
}
              
Below is an extended example that demonstrates how Web cells allow
the creation of reusable Web abstractions without requiring global
transformations of the program into continuation or store passing style.
@racketmod[
 web-server/insta

 (define (start initial-request)
   (define counter1 (make-counter))
   (define counter2 (make-counter))
   (define include1 (include-counter counter1))
   (define include2 (include-counter counter2))
   (send/suspend/dispatch
    (lambda (embed/url)
      (response/xexpr
       `(html
         (body (h2 "Double Counters")
               (div (h3 "First")
                    ,(include1 embed/url))
               (div (h3 "Second")
                    ,(include2 embed/url))))))))

(define (make-counter)
  (make-web-cell 0))

(define (include-counter a-counter)
  (call-with-current-continuation
   (λ (k)
     (let loop ()
       (k
        (lambda (embed/url)
          `(div (h3 ,(number->string (web-cell-ref a-counter)))
                (a ([href
                     ,(embed/url
                       (lambda _
                         ; A new frame has been created
                         (define last (web-cell-ref a-counter))
                         ; We can inspect the value at the parent
                         (web-cell-shadow a-counter (add1 last))
                         ; The new frame has been modified
                         (loop)))])
                   "+"))))))
   servlet-prompt))
]
}
