#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/eval
          mzlib/process
          "guide-utils.ss"
          (for-label scheme/tcp
                     scheme/serialize
                     scheme/port))

@(define io-eval (make-base-eval))

@(define (twocolumn a b)
   (make-table #f
     (list (list (make-flow (list a))
                 (make-flow (list (make-paragraph (list (hspace 1)))))
                 (make-flow (list b))))))
@(interaction-eval #:eval io-eval (print-hash-table #t))

@title[#:tag "i/o" #:style 'toc]{Input and Output}

A Scheme @deftech{port} represents an input or output stream, such as
a file, a terminal, a TCP connection, or an in-memory string. More
specifically, an @defterm{input port} represents a stream from which a
program can read data, and an @defterm{output port} represents a
stream for writing data.

@local-table-of-contents[]

@;------------------------------------------------------------------------
@section{Varieties of Ports}

Various functions create various kinds of ports. Here are a few
examples:

@itemize[

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 @item{@bold{Files:} The @scheme[open-output-file] function opens a
  file for writing, and @scheme[open-input-file] opens a file for
  reading.

@(interaction-eval #:eval io-eval (define old-dir (current-directory)))
@(interaction-eval #:eval io-eval (current-directory (find-system-path 'temp-dir)))
@(interaction-eval #:eval io-eval (when (file-exists? "data") (delete-file "data")))

@examples[
#:eval io-eval
(define out (open-output-file "data"))
(display "hello" out)
(close-output-port out)
(define in (open-input-file "data"))
(read-line in)
(close-input-port in)
]

If a file exists already, then @scheme[open-output-file] raises an
exception by default. Supply an option like @scheme[#:exists
'truncate] or @scheme[#:exists 'update] to re-write or update the
file:

@examples[
#:eval io-eval
(define out (open-output-file "data" #:exists 'truncate))
(display "howdy" out)
(close-output-port out)
]

Instead of having to match @scheme[open-input-file] and
@scheme[open-output-file] calls, most Scheme programmers will instead
use @scheme[call-with-output-file], which takes a function to call
with the output port; when the function returns, the port is closed.

@examples[
        #:eval io-eval
(call-with-output-file "data"
                        #:exists 'truncate
                        (lambda (out)
                          (display "hello" out)))
(call-with-input-file "data"
                      (lambda (in)
                        (read-line in)))
]

@(interaction-eval #:eval io-eval (when (file-exists? "data") (delete-file "data")))
@(interaction-eval #:eval io-eval (current-directory old-dir))}

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 @item{@bold{Strings:} The @scheme[open-output-string] function creates
 a port that accumulates data into a string, and @scheme[get-output-string]
 extracts the accumulated string. The @scheme[open-input-string] function
 creates a port to read from a string.

  @examples[
  #:eval io-eval
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
  #:eval io-eval
  (eval:alts (define server (tcp-listen 12345)) (void))
  (eval:alts (define-values (c-in c-out) (tcp-connect "localhost" 12345)) (void))
  (eval:alts (define-values (s-in s-out) (tcp-accept server))
             (begin (define-values (s-in c-out) (make-pipe))
                    (define-values (c-in s-out) (make-pipe))))
  (display "hello\n" c-out)
  (close-output-port c-out)
  (read-line s-in)
  (read-line s-in)
  ]}

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 @item{@bold{Process Pipes:} The @scheme[subprocess] function runs a new
  process at the OS level and returns ports that correspond to the
  subprocess's stdin, stdout, and stderr. (The first three arguments
  can be certain kinds of existing ports to connect directly to the
  subprocess, instead of creating new ports.)

  @examples[
  #:eval io-eval
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
  #:eval io-eval
  (define-values (in out) (make-pipe))
  (display "garbage" out)
  (close-output-port out)
  (read-line in)
 ]}

]

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
#:eval io-eval
(display "Hi")
(code:line (display "Hi" (current-output-port)) (code:comment @#,t{the same}))
]

If you start the @exec{mzscheme} program in a terminal, then the
current input, output, and error ports are all connected to the
terminal. More generally, they are connected to the OS-level stdin,
stdout, and stderr. In this guide, the examples show output written to
stdout in purple, and output written to stderr in red italics.

@defexamples[
#:eval io-eval
(define (swing-hammer)
  (display "Ouch!" (current-error-port)))
(swing-hammer)
]

The current-port functions are actually parameters, which means that
their values can be set with @scheme[parameterize].

@examples[
#:eval io-eval
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

@itemize[

 @item{ @scheme[write], which prints a value in the same way that is it
        printed for a @tech{REPL} result; and }

 @item{@scheme[display], which tends to reduce a value to just its
       character or byte content---at least for those datatypes that
       are primarily about characters or bytes, otherwise it falls
       back to the same output as @scheme[write].}

]

Here are some examples using each:

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
@scheme[display]s the next argument, while @litchar{~s}
@scheme[write]s the next argument.

@defexamples[
#:eval io-eval
(define (deliver who what)
  (printf "Value for ~a: ~s" who what))
(deliver "John" "string")
]

An advantage of @scheme[write], as opposed to @scheme[display], is
that many forms of data can be read back in using @scheme[read].

@examples[
#:eval io-eval
(define-values (in out) (make-pipe))
(write "hello" out)
(read in)
(write '("alphabet" soup) out)
(read in)
(write #hash((a . "apple") (b . "banana")) out)
(read in)
]

@; ----------------------------------------------------------------------
@section[#:tag "serialization"]{Datatypes and Serialization}

@tech{Prefab} structure types (see @secref["prefab-struct"])
automatically support @deftech{serialization}: they can be written to
an output stream, and a copy can be read back in from an input stream:

@interaction[
(define-values (in out) (make-pipe))
(write #s(sprout bean) out)
(read in)
]

Other structure types created by @scheme[define-struct], which offer
more abstraction than @tech{prefab} structure types, normally
@scheme[write] either using @schemeresultfont{#<....>} notation (for
opaque structure types) or using @schemeresultfont{#(....)} vector
notation (for transparent structure types). In neither can can the
result be read back in as an instance of the structure type:

@interaction[
(define-struct posn (x y))
(write (make-posn 1 2))
(define-values (in out) (make-pipe))
(write (make-posn 1 2) out)
(read in)
]

@interaction[
(define-struct posn (x y) #:transparent)
(write (make-posn 1 2))
(define-values (in out) (make-pipe))
(write (make-posn 1 2) out)
(define v (read in))
v
(posn? v)
(vector? v)
]

The @scheme[define-serializable-struct] form defines a structure type
that can be @scheme[serialize]d to a value that can be printed using
@scheme[write] and restored via @scheme[read]. The @scheme[serialize]d
result can be @scheme[deserialize]d to get back an instance of the
original structure type. The serialization form and functions are
provided by the @schememodname[scheme/serialize] library.

@examples[
(require scheme/serialize)
(define-serializable-struct posn (x y) #:transparent)
(deserialize (serialize (make-posn 1 2)))
(write (serialize (make-posn 1 2)))
(define-values (in out) (make-pipe))
(write (serialize (make-posn 1 2)) out)
(deserialize (read in))
]

In addition to the names bound by @scheme[define-struct],
@scheme[define-serializable-struct] binds an identifier with
deserialization information, and it automatically @scheme[provide]s
the deserialization identifier from a module context. This
deserialization identifier is accessed reflectively when a value is
deserialized.

@; ----------------------------------------------------------------------
@section{Bytes, Characters, and Encodings}

Functions like @scheme[read-line], @scheme[read], @scheme[display],
and @scheme[write] all work in terms of @tech{characters} (which
correspond to Unicode scalar values). Conceptually, they are
implemented in terms of @scheme[read-char] and @scheme[write-char].

More primitively, ports read and write @tech{bytes}, instead of
@tech{characters}. The functions @scheme[read-byte] and
@scheme[write-byte] read and write raw bytes. Other functions, such as
@scheme[read-bytes-line], build on top of byte operations instead of
character operations.

In fact, the @scheme[read-char] and @scheme[write-char] functions are
conceptually implemented in terms of @scheme[read-byte] and
@scheme[write-byte]. When a single byte's value is less than 128, then
it corresponds to an ASCII character. Any other byte is treated as
part of a UTF-8 sequence, where UTF-8 is a particular standard way of
encoding Unicode scalar values in bytes (which has the nice property
that ASCII characters are encoded as themselves). Thus, a single
@scheme[read-char] may call @scheme[read-byte] multiple times, and a
single @scheme[write-char] may generate multiple output bytes.

The @scheme[read-char] and @scheme[write-char] operations
@emph{always} use a UTF-8 encoding. If you have a text stream that
uses a different encoding, or if you want to generate a text stream in
a different encoding, use @scheme[reencode-input-port] or
@scheme[reencode-output-port]. The @scheme[reencode-input-port]
function converts an input stream from an encoding that you specify
into a UTF-8 stream; that way, @scheme[read-char] sees UTF-8
encodings, even though the original used a different encoding. Beware,
however, that @scheme[read-byte] also sees the re-encoded data,
instead of the original byte stream.

@; ----------------------------------------------------------------------
@section{I/O Patterns}

@(begin
  (define port-eval (make-base-eval))
  (interaction-eval #:eval port-eval (require scheme/port)))

If you want to process individual lines of a file, then you can use
@scheme[for] with @scheme[in-lines]:

@interaction[
(define (upcase-all in)
  (for ([l (in-lines in)])
    (display (string-upcase l))
    (newline)))
(upcase-all (open-input-string
             (string-append
              "Hello, World!\n"
              "Can you hear me, now?")))
]

If you want to determine whether ``hello'' appears in a file, then you
could search separate lines, but it's even easier to simply apply a
regular expression (see @secref["regexp"]) to the stream:

@interaction[
(define (has-hello? in)
  (regexp-match? #rx"hello" in))
(has-hello? (open-input-string "hello"))
(has-hello? (open-input-string "goodbye"))
]

If you want to copy one port into another, use @scheme[copy-port] from
@schememodname[scheme/port], which efficiently transfers large blocks
when lots of data is available, but also transfers small blocks
immediately if that's all that is available:

@interaction[
#:eval port-eval
(define o (open-output-string))
(copy-port (open-input-string "broom") o)
(get-output-string o)
]

@close-eval[port-eval]

@; ----------------------------------------------------------------------

@close-eval[io-eval]
