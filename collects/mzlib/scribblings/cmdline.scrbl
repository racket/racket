#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/cmdline))

@(define-syntax-rule (intro id)
   (begin
    (require (for-label scheme/cmdline))
    (define id (scheme command-line))))
@(intro scheme-command-line)

@mzlib[#:mode title cmdline]

Provides a @scheme[command-line] from that is similar to the one in
@schememodname[scheme/cmdline], but without using keywords.  The
@scheme[parse-command-line] procedure from
@schememodname[scheme/cmdline] is re-exported directly.

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

Like @scheme-command-line from @scheme[scheme/cmdline], but without
keywords in the syntax.}
