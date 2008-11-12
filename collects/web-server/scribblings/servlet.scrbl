#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "servlet"
       #:style 'toc]{Scheme Servlets}

@defmodule[web-server/servlet]
                    
The @web-server allows servlets to be written in Scheme. It
provides the supporting API, described below, for the construction
of these servlets.

@local-table-of-contents[]

@; ------------------------------------------------------------
@section[#:tag "module-servlets"]{Definition}
@(require (for-label "dummy-servlet.ss")) @; to give a binding context

@declare-exporting[#:use-sources (web-server/scribblings/dummy-servlet)]

A @defterm{servlet} is a module that provides the following:

@defthing[interface-version (one-of/c 'v1 'v2 'stateless)]{
 A symbol indicating the servlet interface the servlet conforms
 to. This influences the other provided identifiers.
}

@defthing[timeout integer?]{
 Only if @scheme[interface-version] is @scheme['v1].

 This number is used as the @scheme[continuation-timeout] argument to
 a timeout-based continuation manager used for this servlet. (See
 @secref["timeouts.ss"].) (i.e., you do not have a choice of the manager
 for this servlet and will be given a timeout-based manager.)
}

@defthing[manager manager?]{
 Only if @scheme[interface-version] is @scheme['v2].

 The manager for the continuations of this servlet.
}

@defproc[(start [initial-request request?])
         response?]{
 This function is called when an instance of this servlet is started.
 The argument is the HTTP request that initiated the instance.
}
                   
An example version 1 module:
@schememod[
 scheme
 
 (define interface-version 'v1)
 (define timeout (* 60 60 24))
 (define (start req)
   `(html (head (title "Hello World!"))
          (body (h1 "Hi Mom!"))))
 ]

An example version 2 module:
@(require (for-label web-server/managers/none))
@schememod[
 scheme
 (require web-server/managers/none)
 
 (define interface-version 'v2)
 (define manager 
   (create-none-manager
    (lambda (req)
      `(html (head (title "No Continuations Here!"))
             (body (h1 "No Continuations Here!"))))))
 (define (start req)
   `(html (head (title "Hello World!"))
          (body (h1 "Hi Mom!"))))
 ]

An example @scheme['stateless] servlet module:
@schememod[
 web-server
 (define interface-version 'stateless)
 (define (start req)
   `(html (body (h2 "Look ma, no state!"))))
]

@; ------------------------------------------------------------
@section[#:tag "servlet-structs.ss"]{Contracts}
@(require (for-label web-server/servlet/servlet-structs
                     web-server/servlet))

@defmodule[web-server/servlet/servlet-structs]

@filepath{servlet/servlet-structs.ss} provides a number of contracts
for use in servlets.

@defthing[k-url? contract?]{
Equivalent to @scheme[string?]. 
                                       
Example: @scheme["http://localhost:8080/servlets;1*1*20131636/examples/add.ss"]}

@defthing[response-generator? contract?]{
Equivalent to @scheme[(k-url? . -> . response?)].
           
Example: @schemeblock[(lambda (k-url)
                        `(html 
                          (body 
                           (a ([href ,k-url])
                              "Click Me to Invoke the Continuation!"))))]
}

@defthing[expiration-handler/c contract?]{
Equivalent to @scheme[(or/c false/c (request? . -> . response?))].
           
Example: @schemeblock[(lambda (req)
                        `(html (head (title "Expired"))
                               (body (h1 "Expired")
                                     (p "This URL has expired. "
                                        "Please return to the home page."))))]
}

@defthing[embed/url/c contract?]{
Equivalent to @scheme[(((request? . -> . any/c)) (expiration-handler/c) . opt-> . string?)].

This is what @scheme[send/suspend/dispatch] gives to its function argument.
}

@; ------------------------------------------------------------
@section[#:tag "web.ss"]{Web}
@(require (for-label web-server/servlet/web))

@defmodule[web-server/servlet/web]{The
@schememodname[web-server/servlet/web] library provides the primary
functions of interest for the servlet developer.}

@defproc[(send/back [response response?])
         void?]{
 Sends @scheme[response] to the client. No continuation is captured, so the servlet is done.
       
 Example:
 @schemeblock[
  (send/back
   `(html
     (body
      (h1 "The sum is: "
          ,(+ first-number
              second-number)))))
 ]
}

@defthing[current-servlet-continuation-expiration-handler (parameter/c expiration-handler/c)]{
 Holds the @scheme[expiration-handler/c] to be used when a continuation
 captured in this context is expired, then looked up.
 
 Example:
 @schemeblock[
  (parameterize 
      ([current-servlet-continuation-expiration-handler
        (lambda (req)
          `(html (head (title "Custom Expiration!"))))])
    (send/suspend
     ...))
  ]               
}

@defproc[(send/suspend [make-response response-generator?]
                       [exp expiration-handler/c (current-servlet-continuation-expiration-handler)])
         request?]{
 Captures the current continuation, stores it with @scheme[exp] as the expiration
 handler, and binds it to a URL. @scheme[make-response] is called with this URL and
 is expected to generate a @scheme[response?], which is sent to the client. If the
 continuation URL is invoked, the captured continuation is invoked and the request is
 returned from this call to @scheme[send/suspend].
 
 Example:
 @schemeblock[
  (send/suspend
   (lambda (k-url)
     `(html (head (title "Enter a number"))
            (body 
             (form ([action ,k-url])
                   "Enter a number: "
                   (input ([name "number"]))
                   (input ([type "submit"])))))))
  ]
 
 When this form is submitted by the browser, the request will be sent to the URL generated by @scheme[send/suspend].
 Thus, the request will be ``returned'' from @scheme[send/suspend] to the continuation of this call.
}
                  
@defproc[(send/suspend/dispatch [make-response (embed/url/c . -> . response?)])
         any/c]{
 Calls @scheme[make-response] with a function that, when called with a procedure from
 @scheme[request?] to @scheme[any/c] will generate a URL, that when invoked will call
 the function with the @scheme[request?] object and return the result to the caller of
 @scheme[send/suspend/dispatch].
 
 Use @scheme[send/suspend/dispatch] when there are multiple `logical' continuations of a page.
 For example, we could either add to a number or subtract from it:
 @schemeblock[
  (define (count-dot-com i)
    (count-dot-com 
     (send/suspend/dispatch
      (lambda (embed/url)
        `(html 
          (head (title "Count!"))
          (body
           (h2 (a ([href
                    ,(embed/url
                      (lambda (req)
                        (sub1 i)))])
                  "-"))
           (h1 ,(number->string i))
           (h2 (a ([href
                    ,(embed/url
                      (lambda (req)
                        (add1 i)))])
                  "+"))))))))
  ]
 It is very common that the return value of @scheme[send/suspend/dispatch] is irrevelant in
 your application and you may think of it as ``embedding'' value-less callbacks.
}

@defproc[(clear-continuation-table!)
         void?]{
 Calls the servlet's manager's @scheme[clear-continuation-table!] function. Normally, this deletes all the previously
 captured continuations.
}

@defproc[(send/forward [make-response response-generator?]
                       [exp expiration-handler/c (current-servlet-continuation-expiration-handler)])
         request?]{
 Calls @scheme[clear-continuation-table!], then @scheme[send/suspend].
       
 Use this if the user can logically go `forward' in your application, but cannot go backward.
}

@defproc[(send/finish [response response?])
         void?]{
 Calls @scheme[clear-continuation-table!], then @scheme[send/back].
       
 Use this if the user is truly `done' with your application. For example, it may be used to display the post-logout page:
 @schemeblock[
  (send/finish
   `(html (head (title "Logged out"))
          (body (p "Thank you for using the services "
                   "of the Add Two Numbers, Inc."))))
  ]
}

@defproc[(redirect/get)
         request?]{
 Calls @scheme[send/suspend] with @scheme[redirect-to].
       
 This implements the Post-Redirect-Get pattern. 
 Use this to prevent the @onscreen["Refresh"] button from duplicating effects, such as adding items to a database. 
}

@defproc[(redirect/get/forget)
         request?]{
 Calls @scheme[send/forward] with @scheme[redirect-to].
}
                  
@defproc[(adjust-timeout! [t number?])
         void?]{
 Calls the servlet's manager's @scheme[adjust-timeout!] function.
       
 @warning{This is deprecated and will be removed in a future release.}
}
                  
@defproc[(continuation-url? [u url?])
         (or/c false/c (list/c number? number? number?))]{
 Checks if @scheme[u] is a URL that refers to a continuation, if so
 returns the instance id, continuation id, and nonce.
}
                  
@; ------------------------------------------------------------
@section[#:tag "helpers.ss"]{Helpers}
@(require (for-label web-server/servlet/helpers))

@defmodule[web-server/servlet/helpers]

@defproc[(with-errors-to-browser [send/finish-or-back (response? . -> . request?)]
                                 [thunk (-> any)])
         any]{
 Calls @scheme[thunk] with an exception handler that generates an HTML error page
 and calls @scheme[send/finish-or-back].
 
 Example:
 @schemeblock[
  (with-errors-to-browser
   send/back
   (lambda ()
     (/ 1 (get-number (request-number)))))
 ]
}

@; ------------------------------------------------------------
@section[#:tag "web-cells.ss"]{Web Cells}
@(require (for-label web-server/servlet/web-cells))

@defmodule[web-server/servlet/web-cells]{The
@schememodname[web-server/servlet/web-cells] library provides the
interface to Web cells.}

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
 Determines if @scheme[v] is a web-cell.
}

@defproc[(make-web-cell [v any/c])
         web-cell?]{
 Creates a web-cell with a default value of @scheme[v].
}

@defproc[(web-cell-ref [wc web-cell?])
         any/c]{
 Looks up the value of @scheme[wc] found in the nearest
 frame.
}

@defproc[(web-cell-shadow [wc web-cell?]
                          [v any/c])
         void]{
 Binds @scheme[wc] to @scheme[v] in the current frame, shadowing any
 other bindings to @scheme[wc] in the current frame.
}
              
Below is an extended example that demonstrates how Web cells allow
the creation of reusable Web abstractions without requiring global
transformations of the program into continuation or store passing style.
@schememod[
 web-server/insta

 (define (start initial-request)
  (define counter1 (make-counter))
  (define counter2 (make-counter))
  (define include1 (include-counter counter1))
  (define include2 (include-counter counter2))
  (send/suspend/dispatch
   (lambda (embed/url)
     `(html 
       (body (h2 "Double Counters")
             (div (h3 "First")
                  ,(include1 embed/url))
             (div (h3 "Second")
                  ,(include2 embed/url)))))))

(define (make-counter)
  (make-web-cell 0))

(define (include-counter a-counter)
  (let/cc k
    (let loop ()
      (k
       (lambda (embed/url)
         `(div (h3 ,(number->string (web-cell-ref a-counter)))
               (a ([href 
                    ,(embed/url
                      (lambda _
                        @code:comment{A new frame has been created}
                        (define last (web-cell-ref a-counter))
                        @code:comment{We can inspect the value at the parent}
                        (web-cell-shadow a-counter (add1 last))
                        @code:comment{The new frame has been modified}
                        (loop)))])
                  "+")))))))
]

@; ------------------------------------------------------------
@section[#:tag "setup.ss"]{Setup}
@(require (for-label web-server/servlet/setup))

@defmodule[web-server/servlet/setup]

This module is used internally to build and load servlets. It may be useful to those who are trying to extend the server.

@defproc[(make-v1.servlet [directory path?]
                          [timeout integer?]
                          [start (request? . -> . response?)])
         servlet?]{
 Creates a version 1 servlet that uses @scheme[directory] as its current directory, a timeout manager with a @scheme[timeout] timeout, and @scheme[start] as the request handler.
}

@defproc[(make-v2.servlet [directory path?]
                          [manager manager?]
                          [start (request? . -> . response?)])
         servlet?]{
 Creates a version 2 servlet that uses @scheme[directory] as its current directory, a @scheme[manager] as the continuation manager, and @scheme[start] as the request handler.
}
 
@defproc[(make-stateless.servlet [directory path?]
                                 [start (request? . -> . response?)])
         servlet?]{
 Creates a stateless @schememodname[web-server] servlet that uses @scheme[directory] as its current directory and @scheme[start] as the request handler.
}
                  
@defthing[default-module-specs (listof module-path?)]{
 The modules that the Web Server needs to share with all servlets.
}

@defthing[path->servlet/c contract?]{
Equivalent to @scheme[(path? . -> . servlet?)].
}

@defproc[(make-default-path->servlet 
          [#:make-servlet-namespace
           make-servlet-namespace
           make-servlet-namespace?
           (make-make-servlet-namespace)]
          [#:timeouts-default-servlet
           timeouts-default-servlet
           integer?
           30])
         path->servlet/c]{
 Constructs a procedure that loads a servlet from the path in a namespace created with @scheme[make-servlet-namespace],
 using a timeout manager with @scheme[timeouts-default-servlet] as the default timeout (if no manager is given.)
} 