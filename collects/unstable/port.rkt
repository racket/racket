#lang racket/base

(provide null-output-port)

;; taken from section 12.1.9 of the reference
;; -- stamourv
(define null-output-port
  (make-output-port
   'null
   always-evt
   (lambda (s start end non-block? breakable?) (- end start))
   void
   (lambda (special non-block? breakable?) #t)
   (lambda (s start end) (wrap-evt
                          always-evt
                          (lambda (x)
                            (- end start))))
   (lambda (special) always-evt)))
