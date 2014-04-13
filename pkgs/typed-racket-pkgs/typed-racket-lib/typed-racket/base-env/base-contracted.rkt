#lang racket/base

;; This file provides Typed Racket bindings for values that need
;; contract protection, even in typed code.

(require "../utils/utils.rkt"
         (utils any-wrap)
         (prefix-in c: racket/contract)
         (rename-in
          racket/base
          [default-continuation-prompt-tag -default-continuation-prompt-tag])
         (for-syntax racket/base
                     (env env-req)))

(provide default-continuation-prompt-tag)

;; default tag should use Any wrappers
(define default-continuation-prompt-tag
  (c:contract (c:-> (c:prompt-tag/c any-wrap/c #:call/cc any-wrap/c))
              -default-continuation-prompt-tag
              ;; TODO: we actually want to be able to specify that the
              ;;       "contract from" party is not the untyped party
              ;;       here, but that's not currently possible
              'untyped 'typed))

(begin-for-syntax
  (add-mod! (variable-reference->module-path-index (#%variable-reference))))

;; Set up a #%type-decl manually to avoid the overhead of bringing in
;; the "extra-env-lang.rkt" module
(begin-for-syntax
  (module* #%type-decl #f
    (#%plain-module-begin
     (require typed-racket/env/global-env
              typed-racket/types/abbrev
              typed-racket/rep/type-rep)
     (register-type
      (quote-syntax default-continuation-prompt-tag)
      ;; TODO: we actually want the type
      ;;       for the handler (->* (list) Univ ManyUniv)
      ;;       but the prompt tag contract doesn't quite
      ;;       support this (it needs a #:rest argument)
      ;;
      ;;       Also, this type works better with inference.
      (-> (make-Prompt-Tagof Univ (-> Univ ManyUniv)))))))
