(module launcher-bootstrap mzscheme
  (require (lib "string.ss")
           (lib "file.ss"))
  
  (define argv (current-command-line-arguments))
  ;; skip first six
  (define program-argv (list->vector (cddr (cddddr (vector->list argv)))))
  
  (define init-code (read-from-string (vector-ref argv 0)))
  (define program-filename (vector-ref argv 1))
  (define language-module-spec (read-from-string (vector-ref argv 2)))
  (define transformer-module-spec (read-from-string (vector-ref argv 3)))
  (define use-require/copy? (read-from-string (vector-ref argv 4)))
  (define to-be-copied-module-specs (read-from-string (vector-ref argv 5)))
  
  (define to-be-copied-module-names
    (let ([get-name
           (λ (spec)
             (if (symbol? spec)
                 spec
                 ((current-module-name-resolver) spec #f #f)))])
      (map get-name to-be-copied-module-specs)))
  
  (define init-code-tmp-filename (make-temporary-file "drs-launcher-init~a"))
  (define-values (_1 init-code-mod-name _2) (split-path init-code-tmp-filename))
  
  (set-car! (cdr init-code) (string->symbol (path->string init-code-mod-name)))
  
  (call-with-output-file init-code-tmp-filename
    (λ (port)
      (write init-code port))
    'truncate 'text)
  
  (define init-code-proc (dynamic-require init-code-tmp-filename 'init-code))
  
  (define original-namespace (current-namespace))
  (current-namespace (make-namespace 'empty))
  (for-each (λ (x) (namespace-attach-module original-namespace x))
            to-be-copied-module-names)
  (namespace-set-variable-value! 'argv program-argv)
  (current-command-line-arguments program-argv)
  (namespace-require language-module-spec)
  (when use-require/copy?
    (namespace-require/copy language-module-spec))
  (when transformer-module-spec
    (namespace-transformer-require transformer-module-spec))
  
  (init-code-proc)
  
  ;; safe to do this earlier?
  (delete-file init-code-tmp-filename)
  
  (load program-filename))