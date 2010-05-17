(module language-info "private/pre-base.rkt"

  (provide get-info)
  
  (define (get-info data)
    (lambda (key default)
      (case key
        [(configure-runtime)
         '(#(racket/runtime-config configure #f))]
        [else default]))))

