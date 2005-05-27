
(module interactive-value-port mzscheme
  (require (lib "pretty.ss")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           "syntax-browser.ss")
  (provide set-interactive-display-handler
           set-interactive-write-handler
           set-interactive-print-handler)

  (define op (current-output-port))
  (define (oprintf . x) (apply fprintf op x))
  
  (define (set-interactive-display-handler port)
    (let ([original-port-display-handler (port-display-handler port)])
      (port-display-handler
       port
       (λ (val port)
         (cond
           [(string? val) (original-port-display-handler val port)]
           [else
            (do-printing pretty-display val port)])))))
  
  (define (set-interactive-write-handler port)
    (port-write-handler
     port
     (λ (val port)
       (do-printing pretty-print val port))))
  
  (define (set-interactive-print-handler port)
    (port-print-handler
     port
     (λ (val port)
       (do-printing pretty-print val port))))
            
  (define (use-number-snip? x)
    (and #f
         (number? x)
         (exact? x)
         (real? x)
         (not (integer? x))))
  
  (define (do-printing pretty value port)
    (parameterize ([pretty-print-columns 'infinity]
                   [pretty-print-size-hook
                    (λ (value display? port)
                      (cond
                        [(is-a? value snip%) 1]
                        ;[(use-number-snip? value) 1]
                        [(syntax? value) 1]
                        [else #f]))]
                   [pretty-print-print-hook
                    (λ (value display? port)
                      (cond
                        [(is-a? value snip%)
                         (write-special value port)
                         1]
                        #;
                        [(use-number-snip? value)
                         (write-special
                          (number-snip:make-repeating-decimal-snip value #f)
                          port)
                         1]
                        [(syntax? value)
                         (write-special (render-syntax/snip value))]
                        [else (void)]))])
      (pretty value port))))
