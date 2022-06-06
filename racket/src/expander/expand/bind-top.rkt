#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "root-expand-context.rkt"
         "context.rkt"
         "def-id.rkt"
         "dup-check.rkt"
         "use-site.rkt")

;; When compiling `(define-values (x) ...)` at the top level, we need
;; `x` to have a binding to give the definition an expand-time meaning.
;; We don't want that binding to take effect outside of evaluation,
;; however; the permanent binding should happen when the
;; `define-values` for is evaluated. So, we use a distinct scope that
;; effectively hides the binding from tasks other than expansion.
;;
;; Note that the `x` binding for `(define-values (x) ...)` is not
;; meant to bind in `...`, even at expansion time. An earlier version
;; of the expander tried to do that on the grounds that it seemed
;; useful, but it didn't work out. (See "the top level is hopeless".)
;;
;; See also "def-id.rkt".

(provide as-expand-time-top-level-bindings)

(define (as-expand-time-top-level-bindings ids s ctx)
  (define top-level-bind-scope (root-expand-context-top-level-bind-scope ctx))
  (define tl-ids
    (for/list ([id (in-list ids)])
      (remove-use-site-scopes id ctx)))
  (check-no-duplicate-ids tl-ids (expand-context-phase ctx) s)
  (define tmp-bind-ids
    (for/list ([id (in-list tl-ids)])
      (add-scope id top-level-bind-scope)))
  (values tl-ids
          (select-defined-syms-and-bind!/ctx tmp-bind-ids ctx)))
