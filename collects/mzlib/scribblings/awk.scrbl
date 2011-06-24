#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/awk
                     scheme/contract))

@mzlib[#:mode title awk]

@defform/subs[
#:literals (after range / => :range range: :range: else)
(awk next-record-expr
     (record field-id ...)
     maybe-counter
     ((state-variable init-expr) ...)
     maybe-continue
  clause ...)
([maybe-counter code:blank
                id]
 [maybe-continue code:blank
                 id]
 [clause (test body ...+)
         (test => procedure-expr)
         (/ regexp-str / (id-or-false ...+) body ...+)
         (range excl-start-test excl-stop-test body ...+)
         (:range incl-start-test excl-stop-test body ...+)
         (range: excl-start-test incl-stop-test body ...+)
         (:range: incl-start-test incl-stop-test body ...+)
         (else body ...+)
         (after body ...+)]
 [test integer
       regexp-string
       expr]
 [excl-start-test test]
 [excl-stop-test test]
 [incl-start-test test]
 [incl-stop-test test]
 [id-or-false id
              #f])]{

The @racket[awk] macro from Scsh @cite["Shivers06"]. In addition to
@racket[awk], the Scsh-compatible procedures @racket[match:start],
@racket[match:end], @racket[match:substring], and @racket[regexp-exec]
are defined. These @racketidfont{match:} procedures must be used to
extract match information in a regular expression clause when using
the @racket[=>] form.  }

@deftogether[(
@defproc[(match:start [rec ....]
                      [which exact-nonnegative-integer? 0])
         exact-nonnegative-integer?]
@defproc[(match:end   [rec ....]
                      [which exact-nonnegative-integer? 0])
         exact-nonnegative-integer?]
@defproc[(match:substring   
                      [rec ....]
                      [which exact-nonnegative-integer? 0])
         string?]
)]{

Extracts a start position, end position, or substring corresponding to
a match. The first argument is the value supplied to the procedure
after @racket[=>] in a @racket[awk] clause or the result of
@racket[regexp-exec].}


@defproc[(regexp-exec [re (or/c string? regexp?)] [s string?])
         (or/c .... false/c)]{

Matches a regexp to a string, returning a record compatible with
@racket[match:start], etc.}
