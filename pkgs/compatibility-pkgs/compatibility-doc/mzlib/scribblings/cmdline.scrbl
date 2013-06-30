#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/cmdline))

@(define-syntax-rule (intro id)
   (begin
     (require (for-label racket/cmdline))
     (define id (racket command-line))))
@(intro racket-command-line)

@mzlib[#:mode title cmdline]

@deprecated[@racketmodname[racket/cmdline]]{}

Provides a @racket[command-line] from that is similar to the one in
@racketmodname[racket/cmdline], but without using keywords.  The
@racket[parse-command-line] procedure from
@racketmodname[racket/cmdline] is re-exported directly.

@defform/subs[
#:literals (multi once-each once-any final help-labels args =>)
(command-line program-name-expr argv-expr clause ...)
([clause (multi flag-spec ...)
         (once-each flag-spec ...)
         (once-any flag-spec ...)
         (final flag-spec ...)
         (help-labels string ...)
         (args arg-formals body-expr ...+)
         (=> finish-proc-expr arg-help-expr help-proc-expr 
             unknown-proc-expr)]
 [flag-spec (flags id ... help-str ...+ body-expr ...+) 
            (flags => handler-expr help-expr)]
 [flags flag-string 
        (flag-string ...+)]
 [arg-formals id
              (id ...)
              (id ...+ . id)])]{

Like @racket-command-line from @racket[racket/cmdline], but without
keywords in the syntax.}
