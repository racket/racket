#lang racket/base

(provide startup)

(require racket/file)

(define (read-from-string s) (read (open-input-string s)))

(define (startup)
  (define argv (current-command-line-arguments))
  ;; skip first six
  (define program-argv (list->vector (cddr (cddddr (vector->list argv)))))
  
  (define init-code (read-from-string (vector-ref argv 0)))
  (define program-filename (vector-ref argv 1))
  (define language-module-spec (read-from-string (vector-ref argv 2)))
  (define transformer-module-spec (read-from-string (vector-ref argv 3)))
  (define use-require/copy? (read-from-string (vector-ref argv 4)))
  
  (define init-code-tmp-filename (make-temporary-file "drs-launcher-init~a"))
  (define-values (_1 init-code-mod-name _2) (split-path init-code-tmp-filename))
  
  (define stupid-internal-define-syntax2
    (set! init-code (cons (car init-code)
                          (cons (string->symbol (path->string init-code-mod-name))
                                (cddr init-code)))))
  
  (define stupid-internal-define-syntax1
    (call-with-output-file init-code-tmp-filename
      (λ (port)
        (write init-code port))
      #:exists 'truncate #:mode 'text))
  
  (define init-code-proc (dynamic-require init-code-tmp-filename 'init-code))
  
  (namespace-set-variable-value! 'argv program-argv)
  (current-command-line-arguments program-argv)
  (when language-module-spec
    (namespace-require language-module-spec))
  (when use-require/copy?
    (namespace-require/copy language-module-spec))
  (when transformer-module-spec
    (namespace-require `(for-syntax ,transformer-module-spec)))
  
  (init-code-proc)
  
  ;; safe to do this earlier?
  (delete-file init-code-tmp-filename)
  
  (load program-filename))
