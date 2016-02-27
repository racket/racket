#lang racket/base
(require racket/string
         racket/contract/base
         racket/list)

(define-struct (url-exception exn:fail) ())
(define (-url-exception? x)
  (or (url-exception? x)

      ;; two of the errors that string->url can raise are
      ;; now contract violations instead of url-expcetion
      ;; structs. since only the url-exception? predicate
      ;; was exported, we just add this in to the predicate
      ;; to preserve backwards compatibility
      (and (exn:fail:contract? x)
           (regexp-match? #rx"^string->url:" (exn-message x)))))

(provide (struct-out url-exception))
(provide/contract (-url-exception? (any/c . -> . boolean?)))
