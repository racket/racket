#lang scheme

(require (lib "matrix-sig.ss" "htdp")
         (lib "matrix-unit.ss" "htdp")
         (lib "matrix-render.ss" "htdp"))

(define render@
  (unit (import)
        (export matrix-render^)
        (define-struct invisible (matrix))
	(define visible? invisible?)
        (define make-visible make-invisible)
        (define visible-matrix invisible-matrix)))

(define invisible-matrix@
  (compound-unit
    (import)
    (export m)
    (link (((r : matrix-render^)) render@)
          (((m : matrix^)) matrix@ r))))

(define-values/invoke-unit invisible-matrix@ (import) (export matrix^))

(provide-signature-elements matrix^)
