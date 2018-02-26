#lang racket/base

(provide interp)

;; Compilation produces a matcher function; see "match.rkt"
(define (interp m      ; the compiled matcher function
                s      ; input bytes or lazy-bytes
                pos    ; starting seach position, can be > `start`, must be < `limit`
                start  ; input start in the sense of `^`; don't read before this
                limit/end ; don't read past `limit`; `end` corresponds to `$` and can be < `limit`
                state) ; vector where group position-pair matches are installed
  ;; The search `pos` can be greater than `start` due to prefix bytes
  ;; passed to `regexp-match`.
  ;; The search `limit` and `end` start out the same, but `limit`
  ;; can be less than `end` for a lookbehind match.
  (m s pos start limit/end limit/end state null))

