#lang scribble/doc
@(require "mz.rkt")

@(define sp-eval (make-base-eval))
@(interaction-eval #:eval sp-eval (require racket/list))

@title[#:tag "stringport"]{String Ports}

A @deftech{string port} reads or writes from a @tech{byte string}. An
input @tech{string port} can be created from either a @tech{byte string}
or a @tech{string}; in the latter case, the @tech{string} is effectively
converted to a @tech{byte string} using @racket[string->bytes/utf-8]. An
output @tech{string port} collects output into a @tech{byte string}, but
@racket[get-output-string] conveniently converts the accumulated bytes
to a @tech{string}.

Input and output @tech{string ports} do not need to be explicitly
closed. The @racket[file-position] procedure works for @tech{string
ports} in position-setting mode.

@refalso["bytestrings"]{bytestrings}

@defproc[(open-input-bytes [bstr bytes?] [name any/c 'string]) input-port?]{

Creates an input @tech{string port} that reads characters from
@racket[bstr] (see @secref["bytestrings"]). Modifying @racket[bstr]
afterward does not affect the byte stream produced by the port. The
optional @racket[name] argument is used as the name for the returned
port.}

@examples[#:eval sp-eval
  (define sp (open-input-bytes #"(apples 42 day)"))
  (define sexp1 (read sp))
  (first sexp1)
  (rest sexp1)
  (read-line (open-input-bytes
              #"the cow jumped over the moon\nthe little dog\n"))
]

@refalso["strings"]{strings}

@defproc[(open-input-string [str string?] [name any/c 'string]) input-port?]{

Creates an input @tech{string port} that reads bytes from the UTF-8
encoding (see @secref["encodings"]) of @racket[str]. The optional
@racket[name] argument is used as the name for the returned port.}

@examples[#:eval sp-eval
  (define sp (open-input-string "(λ (x) x)"))
  (read sp)
  (define names (open-input-string "Günter Harder\nFrédéric Paulin\n"))
  (read-line names)
  (read-line names)]

@defproc[(open-output-bytes [name any/c 'string]) output-port?]{

Creates an output @tech{string port} that accumulates the output into a
byte string. The optional @racket[name] argument is used as the name for
the returned port.}

@examples[ #:eval sp-eval
  (define op1 (open-output-bytes))
  (write '((1 2 3) ("Tom" "Dick") ('a 'b 'c)) op1)
  (get-output-bytes op1)
  (define op2 (open-output-bytes))
  (write "Hi " op2)
  (write "there" op2)
  (get-output-bytes op2)
  (define op3 (open-output-bytes))
  (write-bytes #"Hi " op3)
  (write-bytes #"there" op3)
  (get-output-bytes op3)
]

@defproc[(open-output-string [name any/c 'string]) output-port?]{The
same as @racket[open-output-bytes].}

@examples[ #:eval sp-eval
  (define op1 (open-output-string))
  (write '((1 2 3) ("Tom" "Dick") ('a 'b 'c)) op1)
  (get-output-string op1)
  (define op2 (open-output-string))
  (write "Hi " op2)
  (write "there" op2)
  (get-output-string op2)
  (define op3 (open-output-string))
  (write-string "Hi " op3)
  (write-string "there" op3)
  (get-output-string op3)
]

@defproc[(get-output-bytes [out output-port?]
                           [reset? any/c #f]
                           [start-pos exact-nonnegative-integer? 0]
                           [end-pos exact-nonnegative-integer? #f])
         bytes?]{

Returns the bytes accumulated in the @tech{string port} @racket[out] so
far in a freshly allocated @tech{byte string} (including any bytes
written after the port's current position, if any). The @racket[out]
port must be an output @tech{string port} produced by
@racket[open-output-bytes] (or @racket[open-output-string]) or a
structure whose @racket[prop:output-port] property refers to such an
output port (transitively).

If @racket[reset?] is true, then all bytes are removed from the port,
and the port's position is reset to @racket[0]; if @racket[reset?] is
@racket[#f], then all bytes remain in the port for further
accumulation (so they are returned for later calls to
@racket[get-output-bytes] or @racket[get-output-string]), and the
port's position is unchanged.

The @racket[start-pos] and @racket[end-pos] arguments specify the
range of bytes in the port to return; supplying @racket[start-pos] and
@racket[end-pos] is the same as using @racket[subbytes] on the result
of @racket[get-output-bytes], but supplying them to
@racket[get-output-bytes] can avoid an allocation. The
@racket[end-pos] argument can be @racket[#f], which corresponds to not
passing a second argument to @racket[subbytes].}

@examples[ #:eval sp-eval
  (define op (open-output-bytes))
  (write '((1 2 3) ("Tom" "Dick") ('a 'b 'c)) op)
  (get-output-bytes op)
  (get-output-bytes op #f 3 16)
  (get-output-bytes op #t)
  (get-output-bytes op)]

@defproc[(get-output-string [out output-port?]) string?]{
Returns @racket[(bytes->string/utf-8 (get-output-bytes out) #\?)].}

@examples[
(define i (open-input-string "hello world"))
(define o (open-output-string))
(write (read i) o)
(get-output-string o)
]

@close-eval[sp-eval]
