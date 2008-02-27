#lang scheme/base

(require scheme/promise)
(provide (all-from-out scheme/base scheme/promise))

(define (show x p)
  (let show ([x x])
    (cond [(or (void? x) (not x) (null? x)) (void)]
          [(pair? x) (show (car x)) (show (cdr x))]
          [(promise? x) (show (force x))]
          [(keyword? x) (show (keyword->string x))]
          [(and (procedure? x) (procedure-arity-includes? x 0)) (show (x))]
          ;; display won't work, since it calls us back
          ;; [else (display x p)]
          ;; things that are printed directly
          [(bytes? x)  (write-bytes x p)]
          [(string? x) (write-string x p)]
          [(char? x)   (write-char x p)]
          [(number? x) (write x p)]
          ;; generic fallback
          [else (show (format "~a" x))])))

;; this is too much -- it also changes error messages
;; (global-port-print-handler show)
(port-display-handler (current-output-port) show)

;; the default prints a newline too, avoid that
(current-print display)

;; make it possible to use this language through a repl
;; --> won't work: need an `inside' reader that reads a single expression
;; (require (prefix-in * "text/lang/reader.ss"))
;; (current-prompt-read
;;  (lambda () (parameterize ([read-accept-reader #t]) (*read-syntax))))
