#lang racket/base
(require setup/dirs)

(define (check-path-or-false p)
  (unless (or (not p) (path? p))
    (error 'embed-me35 "no good: ~s" p)))

(define (check-list-of-paths ps)
  (unless (and (list? ps) (andmap path? ps))
    (error 'embed-me35 "no good: ~s" ps)))

;; These directories are not available in a created distibution, but
;; they shouldn't crash:
(check-path-or-false (find-apps-dir))
(check-path-or-false (find-pkgs-dir))
(check-list-of-paths (get-pkgs-search-dirs))
(check-path-or-false (find-links-file))

'ok-35
