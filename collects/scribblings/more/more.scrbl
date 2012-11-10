#lang scribble/doc
@(require scribble/manual
          scribble/urls
          scribble/eval
          (only-in scribble/core link-element)
          "../quick/keep.rkt"
          (only-in xrepl/doc-utils [cmd xreplcmd])
          (for-label scheme
                     racket/enter
                     xrepl
                     readline
                     net/url
                     xml
                     racket/control))

@(begin

(define quick @other-manual['(lib "quick.scrbl" "scribblings/quick")])
(define guide @other-manual['(lib "guide.scrbl" "scribblings/guide")])

(define more-eval (make-base-eval))
(interaction-eval #:eval more-eval
                  (define (show-load re?)
                    (fprintf (current-error-port)
                             " [~aloading serve.rkt]\n" (if re? "re-" ""))))
(interaction-eval #:eval more-eval
                  (define (serve n) void))
(interaction-eval #:eval more-eval
                  (define (show-break)
                    (fprintf (current-error-port) "^Cuser break")))
(interaction-eval #:eval more-eval
                  (define (show-fail n)
                    (error 'tcp-listen
                           "listen on ~a failed (address already in use)"
                           n)))
(interaction-eval #:eval more-eval (require xml net/url))

(define (whole-prog which [last? #f])
  (let ([file (format "step~a.txt" which)])
    (margin-note (keep-file file)
                 "Here's the "
                 (if last?
                     "final program"
                     "whole program so far")
                 " in plain text: "
                 (link file "step " which) ".")))

(define-syntax-rule (REQ m) @racket[(require @#,racketmodname[m])])

)

@title{More: Systems Programming with Racket}

@author["Matthew Flatt"]

In contrast to the impression that @|quick| may give, Racket is
not just another pretty face. Underneath the graphical facade of
DrRacket lies a sophisticated toolbox for managing threads and
processes, which is the subject of this tutorial.

Specifically, we show how to build a secure, multi-threaded,
servlet-extensible, continuation-based web server. We use much more of
the language than in @|quick|, and we expect you to click on syntax or
function names that you don't recognize (which will take you to the
relevant documentation). Beware that the last couple of sections
present material that is normally considered difficult. If you're
still new to Racket and have relatively little programming experience,
you may want to skip to @|guide|.

To get into the spirit of this tutorial, we suggest that you set
DrRacket aside for a moment, and switch to raw @exec{racket} in a
terminal. You'll also need a text editor, such as Emacs, vi, or even
Notepad---any editor will do, but one that supports parenthesis
matching would be helpful. Finally, you'll need a web client, perhaps
Lynx or Firefox.

@margin-note{Of course, if you're already spoiled, you can keep using
DrRacket.}

@; ----------------------------------------------------------------------
@section{Ready...}

@link[url:download-drracket]{Download Racket}, install, and then
start @exec{racket} with no command-line arguments:

@verbatim[#:indent 2]{
  $ racket
  @(regexp-replace #rx"\n+$" (banner) "")
  > 
}

For extra read-eval-print loop support, evaluate @REQ[xrepl]
to enable Readline-based input---assuming that you have GNU Readline
installed on your system---and comma-prefixed meta-commands that
support exploration and development. To have @racketmodname[xrepl]
loaded by default, use the @xreplcmd{install!} command, which updates
your @filepath{~/.racketrc} to load @racketmodname[xrepl] whenever you
start @exec{racket} for interactive evaluation.

@margin-note{Unfortunately, for legal reasons related to GPL vs. LGPL,
  @exec{racket} cannot provide Readline automatically.}

@interaction[
(eval:alts @#,REQ[xrepl] (void))
(eval:alts @#,xreplcmd{install!} (void))
]

@; FIXME: probably needs revisions, and questionable whether readline
@; should be mentioned by itself.  One thing to consider is that with
@; readline it's possible to pretend that the whole thing is one
@; session, whereas xrepl changes the prompt.

If you want @emph{just} readline support in @exec{racket}, evaluate
@REQ[readline], instead, and then use @racket[(install-readline!)]  to
adjust @filepath{~/.racketrc} to load @racketmodname[readline].
Readline is not needed if you're using @racketmodname[xrepl], if
you're running a shell inside Emacs, or if you're on Windows and use a
@exec{cmd} window.

@interaction[
(eval:alts @#,REQ[readline] (void))
(eval:alts (install-readline!) (void))
]

@; ----------------------------------------------------------------------
@section{Set...}

In the same directory where you started @exec{racket}, create a text
file @filepath{serve.rkt}, and start it like this:

@racketmod[
racket

(define (go)
  'yep-it-works)
]

@whole-prog["0"]

@; ----------------------------------------------------------------------
@section{Go!}

Back in @exec{racket}, try loading the file and running @racket[go]:

@margin-note{If you use @racketmodname[xrepl], you can use
  @xreplcmd["enter"]{serve.rkt}.}

@interaction[
#:eval more-eval
(eval:alts (enter! "serve.rkt") (show-load #f))
(eval:alts (go) 'yep-it-works)
]

Try modifying @filepath{serve.rkt}, and then run @racket[(enter!
"serve.rkt")] again to re-load the module, and then check your changes.

@; ----------------------------------------------------------------------
@section{``Hello World'' Server}

We'll implement the web server through a @racket[serve] function that
takes an IP port number for client connections:

@racketblock[
(define (serve port-no)
  ...)
]

The server accepts TCP connections through a @defterm{listener}, which
we create with @racket[tcp-listen]. To make interactive development
easier, we supply @racket[#t] as the third argument to
@racket[tcp-listen], which lets us re-use the port number immediately,
without waiting for a TCP timeout.

@racketblock[
(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  ...)
]

The server must loop to accept connections from the listener:

@racketblock[
(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (loop))
]

Our @racket[accept-and-handle] function accepts a connection using
@racket[tcp-accept], which returns two values: a stream for input from
the client, and a stream for output to the client.

@racketblock[
(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (handle in out)
  (close-input-port in)
  (close-output-port out))
]

To handle a connection, for now, we'll read and discard the request
header, and then write a ``Hello, world!'' web page as the result:

@racketblock[
(define (handle in out)
  (code:comment @#,t{Discard the request header (up to blank line):})
  (regexp-match #rx"(\r\n|^)\r\n" in)
  (code:comment @#,t{Send reply:})
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello, world!</body></html>" out))
]

Note that @racket[regexp-match] operates directly on the input stream,
which is easier than bothering with individual lines.

@whole-prog["1"]

Copy the above three definitions---@racket[serve],
@racket[accept-and-handle], and @racket[handle]---into
@filepath{serve.rkt} and re-load:

@interaction[
#:eval more-eval
(eval:alts (enter! "serve.rkt") (show-load #t))
(eval:alts (serve 8080) (void))
]

Now point your browser to @tt{http://localhost:8080} (assuming that
you used @racket[8080] as the port number, and that the browser is
running on the same machine) to receive a friendly greeting from your
web server.

@; ----------------------------------------------------------------------
@section{Server Thread}

Before we can make the web server respond in more interesting ways, we
need to get a Racket prompt back. Typing Ctl-C in your terminal window
interrupts the server loop:

@margin-note{In DrRacket, instead of typing Ctl-C, click the
@onscreen{Stop} button once.}

@interaction[
#:eval more-eval
(eval:alts (serve 8080) (show-break))
(eval:alts code:blank (void))
]

Unfortunately, we cannot now re-start the server with the same port
number:

@interaction[
#:eval more-eval
(eval:alts (serve 8080) (show-fail 8080))
]

The problem is that the listener that we created with @racket[serve]
is still listening on the original port number.

To avoid this problem, let's put the listener loop in its own thread,
and have @racket[serve] return immediately. Furthermore, we'll have
@racket[serve] return a function that can be used to shut down the
server thread and TCP listener:

@racketblock[
(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))
]

@whole-prog["2"]

Try the new one:

@interaction[
#:eval more-eval
(eval:alts (enter! "serve.rkt") (show-load #t))
(define stop (serve 8081))
]

Your server should now respond to @tt{http://localhost:8081}, but you
can shut down and restart the server on the same port number as often
as you like:

@interaction[
#:eval more-eval
(stop)
(define stop (serve 8081))
(stop)
(define stop (serve 8081))
(stop)
]

@; ----------------------------------------------------------------------
@section{Connection Threads}

In the same way that we put the main server loop into a background
thread, we can put each individual connection into its own thread:

@racketblock[
(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (thread
   (lambda ()
     (handle in out)
     (close-input-port in)
     (close-output-port out))))
]

@whole-prog["3"]

With this change, our server can now handle multiple threads at
once. The handler is so fast that this fact will be difficult to
detect, however, so try inserting @racket[(sleep (random 10))] before
the @racket[handle] call above. If you make multiple connections from
the web browser at roughly the same time, some will return soon, and
some will take up to 10 seconds. The random delays will be independent
of the order in which you started the connections.

@; ----------------------------------------------------------------------
@section{Terminating Connections}

A malicious client could connect to our web server and not send the
HTTP header, in which case a connection thread will idle forever,
waiting for the end of the header. To avoid this possibility, we'd
like to implement a timeout for each connection thread.

One way to implement the timeout is to create a second thread that
waits for 10 seconds, and then kills the thread that calls
@racket[handle]. Threads are lightweight enough in Racket that this
watcher-thread strategy works well:

@racketblock[
(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (define t (thread
              (lambda () 
                (handle in out)
                (close-input-port in)
                (close-output-port out))))
  (code:comment @#,t{Watcher thread:})
  (thread (lambda ()
            (sleep 10)
            (kill-thread t))))
]

This first attempt isn't quite right, because when the thread is
killed, its @racket[in] and @racket[out] streams remain open.  We
could add code to the watcher thread to close the streams as well as
kill the thread, but Racket offers a more general shutdown mechanism:
@defterm{custodians}. A custodian is a kind of container for all
resources other than memory, and it supports a
@racket[custodian-shutdown-all] operation that terminates and closes
all resources within the container, whether they're threads, streams,
or other kinds of limited resources.

Whenever a thread or stream is created, it is placed into the current
custodian as determined by the @racket[current-custodian]
parameter. To place everything about a connection into a custodian, we
@racket[parameterize] all the resource creations to go into a new
custodian:

@margin-note{See @secref[#:doc '(lib "scribblings/guide/guide.scrbl") "parameterize"]
             for an introduction to parameters.}

@racketblock[
(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  (code:comment @#,t{Watcher thread:})
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))
]

With this implementation, @racket[in], @racket[out], and the thread
that calls @racket[handle] all belong to @racket[cust]. In addition,
if we later change @racket[handle] so that it, say, opens a file, then
the file handles will also belong to @racket[cust], so they will be
reliably closed when @racket[cust] is shut down.

In fact, it's a good idea to change @racket[serve] so that it uses a
custodian, too:

@racketblock[
(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))
]

That way, the @racket[main-cust] created in @racket[serve] not only
owns the TCP listener and the main server thread, it also owns every
custodian created for a connection. Consequently, the revised shutdown
procedure for the server immediately terminates all active connections,
in addition to the main server loop.

@whole-prog["4"]

After updating the @racket[serve] and @racket[accept-and-handle]
functions as above, here's how you can simulate a malicious client:

@interaction[
#:eval more-eval
(eval:alts (enter! "serve.rkt") (show-load #t))
(define stop (serve 8081))
(eval:alts (define-values (cin cout) (tcp-connect "localhost" 8081)) (void))
]

Now wait 10 seconds. If you try reading from @racket[cin], which is
the stream that sends data from the server back to the client, you'll
find that the server has shut down the connection:

@interaction[
#:eval more-eval
(eval:alts (read-line cin) eof)
]

Alternatively, you don't have to wait 10 seconds if you explicitly
shut down the server:

@interaction[
#:eval more-eval
(eval:alts (define-values (cin2 cout2) (tcp-connect "localhost" 8081)) (void))
(stop)
(eval:alts (read-line cin2) eof)
]

@; ----------------------------------------------------------------------
@section{Dispatching}

It's finally time to generalize our server's ``Hello, World!''
response to something more useful. Let's adjust the server so that we
can plug in dispatching functions to handle requests to different
URLs.

To parse the incoming URL and to more easily format HTML output, we'll
require two extra libraries:

@racketblock[
(require xml net/url)
]

The @racketmodname[xml] library gives us @racket[xexpr->string], which
takes a Racket value that looks like HTML and turns it into actual
HTML:

@interaction[
#:eval more-eval
(xexpr->string '(html (head (title "Hello")) (body "Hi!")))
]

We'll assume that our new @racket[dispatch] function (to be written)
takes a requested URL and produces a result value suitable to use with
@racket[xexpr->string] to send back to the client:

@racketblock[
(define (handle in out)
  (define req
    (code:comment @#,t{Match the first line to extract the request:})
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    (code:comment @#,t{Discard the rest of the header (up to blank line):})
    (regexp-match #rx"(\r\n|^)\r\n" in)
    (code:comment @#,t{Dispatch:})
    (let ([xexpr (dispatch (list-ref req 1))])
      (code:comment @#,t{Send reply:})
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))
]

The @racketmodname[net/url] library gives us @racket[string->url],
@racket[url-path], @racket[path/param-path], and @racket[url-query]
for getting from a string to parts of the URL that it represents:

@interaction[
#:eval more-eval
(define u (string->url "http://localhost:8080/foo/bar?x=bye"))
(url-path u)
(map path/param-path (url-path u))
(url-query u)
]

We use these pieces to implement @racket[dispatch]. The
@racket[dispatch] function consults a hash table that maps an initial
path element, like @racket["foo"], to a handler function:

@racketblock[
(define (dispatch str-path)
  (code:comment @#,t{Parse the request as a URL:})
  (define url (string->url str-path))
  (code:comment @#,t{Extract the path part:})
  (define path (map path/param-path (url-path url)))
  (code:comment @#,t{Find a handler based on the path's first element:})
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      (code:comment @#,t{Call a handler:})
      (h (url-query url))
      (code:comment @#,t{No handler found:})
      `(html (head (title "Error"))
            (body
             (font ((color "red"))
                   "Unknown page: " 
                   ,str-path)))))

(define dispatch-table (make-hash))
]

With the new @racket[require] import and new @racket[handle],
@racket[dispatch], and @racket[dispatch-table] definitions, our
``Hello World!'' server has turned into an error server. You don't have
to stop the server to try it out. After modifying @filepath{serve.rkt}
with the new pieces, evaluate @racket[(enter! "serve.rkt")] and then
try again to connect to the server. The web browser should show an
``Unknown page'' error in red.

We can register a handler for the @racket["hello"] path like this:

@racketblock[
(hash-set! dispatch-table "hello"
           (lambda (query) 
             `(html (body "Hello, World!"))))
]

@whole-prog["5"]

After adding these lines and evaluating @racket[(enter! "serve.rkt")],
opening @tt{http://localhost:8081/hello} should produce the old
greeting.

@; ----------------------------------------------------------------------
@section{Servlets and Sessions}

Using the @racket[query] argument that is passed to a handler by
@racket[dispatch], a handler can respond to values that a user
supplies through a form.

The following helper function constructs an HTML form. The
@racket[label] argument is a string to show the user. The
@racket[next-url] argument is a destination for the form results. The
@racket[hidden] argument is a value to propagate through the form as a
hidden field. When the user responds, the @racket["number"] field in
the form holds the user's value:

@racketblock[
(define (build-request-page label next-url hidden)
  `(html 
    (head (title "Enter a Number to Add"))
    (body ([bgcolor "white"])
          (form ([action ,next-url] [method "get"])
                ,label
                (input ([type "text"] [name "number"]
                                      [value ""]))
                (input ([type "hidden"] [name "hidden"]
                                        [value ,hidden]))
                (input ([type "submit"] [name "enter"] 
                                        [value "Enter"]))))))
]

Using this helper function, we can create a servlet that generates as
many ``hello''s as a user wants:

@margin-note{See @secref[#:doc '(lib "scribblings/guide/guide.scrbl") "for"]
             for an introduction to forms like @racket[for/list].}

@racketblock[
(define (many query)
  (build-request-page "Number of greetings:" "/reply" ""))

(define (reply query)
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ([i (in-range n)])
                   " hello"))))

(hash-set! dispatch-table "many" many)
(hash-set! dispatch-table "reply" reply)
]

@whole-prog["6"]

As usual, once you have added these to your program, update with
@racket[(enter! "serve.rkt")], and then visit
@tt{http://localhost:8081/many}. Provide a number, and you'll receive
a new page with that many ``hello''s.

@; ----------------------------------------------------------------------
@section{Limiting Memory Use}

With our latest @racket["many"] servlet, we seem to have a new
problem: a malicious client could request so many ``hello''s that the
server runs out of memory. Actually, a malicious client could also
supply an HTTP request whose first line is arbitrarily long.

The solution to this class of problems is to limit the memory use of a
connection. Inside @racket[accept-and-handle], after the definition of
@racket[cust], add the line

@racketblock[(custodian-limit-memory cust (* 50 1024 1024))]

@whole-prog["7"]

We're assuming that 50MB should be plenty for any
servlet. Garbage-collector overhead means that the actual memory use
of the system can be some small multiple of 50 MB. An important
guarantee, however, is that different connections will not be charged
for each other's memory use, so one misbehaving connection will not
interfere with a different one.

So, with the new line above, and assuming that you have a couple of
hundred megabytes available for the @exec{racket} process to use,
you shouldn't be able to crash the web server by requesting a
ridiculously large number of ``hello''s.

Given the @racket["many"] example, it's a small step to a web server
that accepts arbitrary Racket code to execute on the server. In that
case, there are many additional security issues besides limiting
processor time and memory consumption. The
@racketmodname[racket/sandbox] library provides support to managing
all those other issues.

@; ----------------------------------------------------------------------
@section{Continuations}

As a systems example, the problem of implementing a web server exposes
many system and security issues where a programming language can
help. The web-server example also leads to a classic, advanced Racket
topic: @defterm{continuations}. In fact, this facet of a web server
needs @defterm{delimited continuations}, which Racket provides.

The problem solved by continuations is related to servlet sessions and
user input, where a computation spans multiple client connections
@cite["Queinnec00"]. Often, client-side computation (as in AJAX) is the
right solution to the problem, but many problems are best solved with
a mixture of techniques (e.g., to take advantage of the browser's
``back'' button).

As the multi-connection computation becomes more complex, propagating
arguments through @racket[query] becomes increasingly tedious. For
example, we can implement a servlet that takes two numbers to add by
using the hidden field in the form to remember the first number:

@racketblock[
(define (sum query)
  (build-request-page "First number:" "/one" ""))

(define (one query)
  (build-request-page "Second number:"
                      "/two"
                      (cdr (assq 'number query))))

(define (two query)
  (let ([n (string->number (cdr (assq 'hidden query)))]
        [m (string->number (cdr (assq 'number query)))])
    `(html (body "The sum is " ,(number->string (+ m n))))))

(hash-set! dispatch-table "sum" sum)
(hash-set! dispatch-table "one" one)
(hash-set! dispatch-table "two" two)
]

@whole-prog["8"]

While the above works, we would much rather write such computations in
a direct style:

@racketblock[
(define (sum2 query)
  (define m (get-number "First number:"))
  (define n (get-number "Second number:"))
  `(html (body "The sum is " ,(number->string (+ m n)))))

(hash-set! dispatch-table "sum2" sum2)
]

The problem is that @racket[get-number] needs to send an HTML response
back for the current connection, and then it must obtain a response
through a new connection. That is, somehow it needs to convert the
page generated by @racket[build-request-page] into a @racket[query]
result:

@racketblock[
(define (get-number label)
  (define query
    ... (build-request-page label ...) ...)
  (number->string (cdr (assq 'number query))))
]

Continuations let us implement a @racket[send/suspend] operation that
performs exactly that operation. The @racket[send/suspend] procedure
generates a URL that represents the current connection's computation,
capturing it as a continuation. It passes the generated URL to a
procedure that creates the query page; this query page is used as the
result of the current connection, and the surrounding computation
(i.e., the continuation) is aborted. Finally, @racket[send/suspend]
arranges for a request to the generated URL (in a new connection) to
restore the aborted computation.

Thus, @racket[get-number] is implemented as follows:

@racketblock[
(define (get-number label)
  (define query
    (code:comment @#,t{Generate a URL for the current computation:})
    (send/suspend
      (code:comment @#,t{Receive the computation-as-URL here:})
      (lambda (k-url)
        (code:comment @#,t{Generate the query-page result for this connection.})
        (code:comment @#,t{Send the query result to the saved-computation URL:})
        (build-request-page label k-url ""))))
  (code:comment @#,t{We arrive here later, in a new connection})
  (string->number (cdr (assq 'number query))))
]

We still have to implement @racket[send/suspend]. For that task, we
import a library of control operators:

@racketblock[(require racket/control)]

Specifically, we need @racket[prompt] and @racket[abort] from
@racketmodname[racket/control]. We use @racket[prompt] to mark the
place where a servlet is started, so that we can abort a computation
to that point. Change @racket[handle] by wrapping an @racket[prompt]
around the call to @racket[dispatch]:

@racketblock[
(define (handle in out)
  ....
    (let ([xexpr (prompt (dispatch (list-ref req 1)))])
      ....))
]

Now, we can implement @racket[send/suspend]. We use @racket[call/cc]
in the guise of @racket[let/cc], which captures the current
computation up to an enclosing @racket[prompt] and binds that
computation to an identifier---@racket[k], in this case:

@racketblock[
(define (send/suspend mk-page)
  (let/cc k
    ...))
]

Next, we generate a new dispatch tag, and we record the mapping from
the tag to @racket[k]:

@racketblock[
(define (send/suspend mk-page)
  (let/cc k
    (define tag (format "k~a" (current-inexact-milliseconds)))
    (hash-set! dispatch-table tag k)
    ...))
]

Finally, we abort the current computation, supplying instead the page
that is built by applying the given @racket[mk-page] to a URL for the
generated tag:
  
@racketblock[
(define (send/suspend mk-page)
  (let/cc k
    (define tag (format "k~a" (current-inexact-milliseconds)))
    (hash-set! dispatch-table tag k)
    (abort (mk-page (string-append "/" tag)))))
]

When the user submits the form, the handler associated with the form's
URL is the old computation, stored as a continuation in the dispatch
table. Calling the continuation (like a function) restores the old
computation, passing the @racket[query] argument back to that
computation.

@whole-prog["9" #t]

In summary, the new pieces are: @racket[(require racket/control)],
adding @racket[prompt] inside @racket[handle], the definitions of
@racket[send/suspend], @racket[get-number], and @racket[sum2], and
@racket[(hash-set! dispatch-table "sum2" sum2)]. Once you have
the server updated, visit @tt{http://localhost:8081/sum2}.

@; ----------------------------------------------------------------------
@section{Where to Go From Here}

The Racket distribution includes a production-quality web server
that addresses all of the design points mentioned here and more.
To learn more, see the tutorial @other-manual['(lib
"web-server/scribblings/tutorial/continue.scrbl")], the
documentation @other-manual['(lib
"web-server/scribblings/web-server.scrbl")], or the research paper
@cite["Krishnamurthi07"].

Otherwise, if you arrived here as part of an introduction to
Racket, then your next stop is probably @|guide|.

If the topics covered here are the kind that interest you, see also
@secref["concurrency" #:doc '(lib
"scribblings/reference/reference.scrbl")] and @secref["security" #:doc
'(lib "scribblings/reference/reference.scrbl")] in @other-manual['(lib
"scribblings/reference/reference.scrbl")].

Some of this material is based on relatively recent research, and more
information can be found in papers written by the authors of Racket,
including papers on GRacket (formerly ``MrEd'') @cite["Flatt99"],
memory accounting @cite["Wick04"], kill-safe abstractions
@cite["Flatt04"], and delimited continuations @cite["Flatt07"].

@; ----------------------------------------------------------------------

@(bibliography

  (bib-entry #:key "Flatt99"
             #:author "Matthew Flatt, Robert Bruce Findler, Shriram Krishnamurthi, and Matthias Felleisen"
             #:title @elem{Programming Languages as Operating Systems
                          (@emph{or} Revenge of the Son of the Lisp Machine)}
             #:location "International Conference on Functional Programming"
             #:date "1999"
             #:url "http://www.ccs.neu.edu/scheme/pubs/icfp99-ffkf.pdf")

  (bib-entry #:key "Flatt04"
             #:author "Matthew Flatt and Robert Bruce Findler"
             #:title "Kill-Safe Synchronization Abstractions"
             #:location "Programming Language Design and Implementation" 
             #:date "2004"
             #:url "http://www.cs.utah.edu/plt/publications/pldi04-ff.pdf")

  (bib-entry #:key "Flatt07"
             #:author "Matthew Flatt, Gang Yu, Robert Bruce Findler, and Matthias Felleisen"
             #:title "Adding Delimited and Composable Control to a Production Programming Environment"
             #:location "International Conference on Functional Programming"
             #:date "2007"
             #:url "http://www.cs.utah.edu/plt/publications/icfp07-fyff.pdf")

  (bib-entry #:key "Krishnamurthi07"
             #:author "Shriram Krishnamurthi, Peter Hopkins, Jay McCarthy, Paul T. Graunke, Greg Pettyjohn, and Matthias Felleisen"
             #:title "Implementation and Use of the PLT Scheme Web Server"
             #:location @italic{Higher-Order and Symbolic Computation}
             #:date "2007"
             #:url "http://www.cs.brown.edu/~sk/Publications/Papers/Published/khmgpf-impl-use-plt-web-server-journal/paper.pdf")

  (bib-entry #:key "Queinnec00"
             #:author "Christian Queinnec"
             #:title "The Influence of Browsers on Evaluators or, Continuations to Program Web Servers"
             #:location "International Conference on Functional Programming"
             #:date "2000"
             #:url "http://www-spi.lip6.fr/~queinnec/Papers/webcont.ps.gz")

  (bib-entry #:key "Wick04"
             #:author "Adam Wick and Matthew Flatt"
             #:title "Memory Accounting without Partitions"
             #:location "International Symposium on Memory Management"
             #:date "2004"
             #:url "http://www.cs.utah.edu/plt/publications/ismm04-wf.pdf")

)




@close-eval[more-eval]
