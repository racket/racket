#lang racket/base
(require racket/private/truncate-path
         "wrap.rkt"
         "match.rkt")

;; We can't store paths in known-value information, so check through
;; wraps to convert to strings any that we find in source locations.

(provide wrap-truncate-paths)

(define (wrap-truncate-paths e)
  (cond
    [(wrap? e)
     (define orig (unwrap e))
     (define u-e (wrap-truncate-paths orig))
     (define-values (src line col pos span) (wrap-source e))
     (cond
       [(and (not (path-for-some-system? src))
             (eq? orig u-e))
        e]
       [(path-for-some-system? src)
        (reannotate/new-srcloc e u-e (srcloc (truncate-path src) line col pos span))]
       [else
        (reannotate e u-e)])]
    [(pair? e)
     (define a (wrap-truncate-paths (car e)))
     (define d (wrap-truncate-paths (cdr e)))
     (cond
       [(and (eq? a (car e))
             (eq? d (cdr e)))
        e]
       [else (cons a d)])]
    [else e]))
