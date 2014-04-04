#lang racket/base

;; Graphviz support
;; inspired by redex/private/dot.rkt (can't use directly because it uses GUI)

(require racket/system "utils.rkt")

(provide render-dot)

;; these paths are explicitly checked (when find-executable-path
;; fails) because starting drracket from the finder (or the dock)
;; under mac os x generally does not get the path right.
(define dot-paths
  '("/usr/bin"
    "/bin"
    "/usr/local/bin"
    "/opt/local/bin/"))

(define dot.exe (if (eq? (system-type) 'windows) "dot.exe" "dot"))
(define dot
  (with-handlers ([(lambda (e) ; may not have permission
                     (and (exn:fail? e)
                          (regexp-match "access denied" (exn-message e))))
                   (lambda _ #f)])
    (or (find-executable-path dot.exe)
        (ormap (λ (x)
                 (define candidate (build-path x dot.exe))
                 (and (file-exists? candidate) candidate))
               dot-paths))))

(define (render-dot input-file)
  (when (and dot (not (dry-run?)))
    (system (format "~a -Tpdf -O ~a" (path->string dot) input-file))))
