#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "struct.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require[(lib "process.ss")]
@require["guide-utils.ss"]

@define[(twocolumn a b)
        (make-table #f
         (list (list (make-flow (list a))
                     (make-flow (list (make-paragraph (list (hspace 1)))))
                     (make-flow (list b)))))]
@interaction-eval[(print-hash-table #t)]

@title[#:tag "i/o" #:style 'toc]{Input and Output}

A Scheme @defterm{port} represents an input or output stream, such as
a file, a terminal, a TCP connection, or an in-memory string. More
specifically, an @defterm{input port} represents a stream from which a
program can read data, and an @defterm{output port} represents a
stream for writing data.

@local-table-of-contents[]

@;------------------------------------------------------------------------
@section{Varieties of Ports}

Various functions create various kinds of ports. Here are a few
examples:

@itemize{

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 @item{@bold{Files:} The @scheme[open-output-file] function opens a
  file for writing, and @scheme[open-input-file] opens a file for
  reading.

@interaction-eval[(define old-dir (current-directory))]
@interaction-eval[(current-directory (find-system-path 'temp-dir))]
@interaction-eval[(when (file-exists? "data") (delete-file "data"))]

@examples[
(define out (open-output-file "data"))
(display "hello" out)
(close-output-port out)
(define in (open-input-file "data"))
(read-line in)
(close-input-port in)
]

@interaction-eval[(delete-file "data")]

Instead of having to match @scheme[open-input-file] and
@scheme[open-output-file] calls, most Scheme programmers will instead
use @scheme[call-with-output-file], which takes a function to call
with the output port; when the function returns, the port is closed.

@examples[
(call-with-output-file "data"
                        (lambda (out)
                          (display "hello" out)))
(call-with-input-file "data"
                      (lambda (in)
                        (read-line in)))
]

@interaction-eval[(delete-file "data")]
@interaction-eval[(current-directory old-dir)]}

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 @item{@bold{Strings:} The @scheme[open-output-string] function creates
 a port that accumulates data into a string, and @scheme[get-output-string]
 extracts the accumulated string. The @scheme[open-input-string] function
 creates a port to read from a string.

  @examples[
  (define p (open-output-string))
  (display "hello" p)
  (get-output-string p)
  (read-line (open-input-string "goodbye\nfarewell"))
  ]}

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 @item{@bold{TCP Connections:} The @scheme[tcp-connect] function
 creates both an input port and an output port for the client side of
 a TCP communication. The @scheme[tcp-listen] function creates a
 server, which accepts connections via @scheme[tcp-accept].

  @examples[
  (eval:alts (define server (tcp-listen 12345)) (void))
  (eval:alts (define-values (c-in c-out) (tcp-connect "localhost" 12345)) (void))
  (eval:alts (define-values (s-in s-out) (tcp-accept server))
             (begin (define-values (s-in c-out) (make-pipe))
                    (define-values (c-in s-out) (make-pipe))))
  (display "hello\n" c-out)
  (read-line s-in)
  (close-output-port c-out)
  (read-line s-in)
  ]}

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 @item{@bold{Process Pipes:} The @scheme[process] function runs a new
  process at the OS level and returns ports that correspond to the
  subprocess's stdin, stdout, and stderr. (The first three arguments
  can be certain kinds of existing ports to connect directly to the
  subprocess, instead of creating new ports.)

  @examples[
  (eval:alts
   (define-values (p stdout stdin stderr)
     (subprocess #f #f #f "/usr/bin/wc" "-w"))
   (define-values (p stdout stdin stderr)
     (values #f (open-input-string "       3") (open-output-string) (open-input-string ""))))
  (display "a b c\n" stdin)
  (close-output-port stdin)
  (read-line stdout)
  (close-input-port stdout)
  (close-input-port stderr)
  ]}

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 @item{@bold{Internal Pipes:} The @scheme[make-pipe] function returns
 two ports that are ends of a pipe. This kind of pipe is internal to
 Scheme, and not related to OS-level pipes for communicating between
 different processes.

 @examples[
  (define-values (in out) (make-pipe))
  (display "garbage" out)
  (close-output-port out)
  (read-line in)
 ]}

}

@;------------------------------------------------------------------------
@section{Default Ports}

For most simple I/O functions, the target port is an optional
argument, and the default is the @defterm{current input port} or
@defterm{current output port}. Furthermore, error messages are written
to the @defterm{current error port}, which is an output port. The
@scheme[current-input-port], @scheme[current-output-port], and
@scheme[current-error-port] functions return the corresponding current
ports.

@examples[
(display "Hi")
(code:line (display "Hi" (current-output-port)) (code:comment #, @t{the same}))
]

If you start the @exec{mzscheme} program in a terminal, then the
current input, output, and error ports are all connected to the
terminal. More generally, they are connected to the OS-level stdin,
stdout, and stderr. In this guide, the examples show output written to
stdout in purple, and output written to stderr in red italics.

@defexamples[
(define (swing-hammer)
  (display "Ouch!" (current-error-port)))
(swing-hammer)
]

The current-port functions actually @tech{parameters}, which means
that their values can be set with @scheme[parameterize].

@moreguide["parameters"]{parameters}

@examples[
(let ([s (open-output-string)])
  (parameterize ([current-error-port s])
    (swing-hammer)
    (swing-hammer)
    (swing-hammer))
  (get-output-string s))
]

@; ----------------------------------------------------------------------
@section{Reading and Writing Scheme Data}

As noted throughout @secref["datatypes"], Scheme provides two
ways to print an instance of a built-in value:

@itemize{

 @item{ @scheme[write], which prints a value in the same way that is it
        printed for a @tech{REPL} result; and }

 @item{@scheme[display], which tends to reduce a value to just its
       character or byte content---at least for those datatypes that
       are primarily about characters or bytes, otherwise it falls
       back to the same output as @scheme[write].}

}

@twocolumn[

@interaction[
(write 1/2)
(write #\x)
(write "hello")
(write #"goodbye")
(write '|dollar sign|)
(write '("alphabet" soup))
(write write)
]

@interaction[
(display 1/2)
(display #\x)
(display "hello")
(display #"goodbye")
(display '|dollar sign|)
(display '("alphabet" soup))
(display write)
]

]

The @scheme[printf] function supports simple formatting of data and
text. In the format string supplied to @scheme[printf], @litchar{~a}
@scheme[display]s the enxt argument, while @litchar{~s}
@scheme[write]s the next argument.

@defexamples[
(define (deliver who what)
  (printf "Value for ~a: ~s" who what))
(deliver "John" "string")
]

An advantage of @scheme[write] is that many forms of data can be
read back in using @scheme[read].

@examples[
(define-values (in out) (make-pipe))
(write "hello" out)
(read in)
(write '("alphabet" soup) out)
(read in)
(write #hash((a . "apple") (b . "banana")) out)
(read in)
]

@; ----------------------------------------------------------------------
@subsection{Serialization}


@; ----------------------------------------------------------------------
@section{Bytes versus Characters}

