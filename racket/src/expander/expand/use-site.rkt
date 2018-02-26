#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "root-expand-context.rkt")

(provide remove-use-site-scopes)

;; Helper to remove any created use-site scopes from the left-hand
;; side of a definition that was revealed by partial expansion in a
;; definition context; the `s` argument can be syntax of a list
;; of syntax
(define (remove-use-site-scopes s ctx)
  (define use-sites (root-expand-context-use-site-scopes ctx))
  (if (and use-sites
           (pair? (unbox use-sites)))
      (if (syntax? s)
          (remove-scopes s (unbox use-sites))
          (for/list ([id (in-list s)])
            (remove-scopes id (unbox use-sites))))
      s))
