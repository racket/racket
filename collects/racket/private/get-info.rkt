(module get-info "pre-base.ss"

  (provide get-info)
  
  (define (get-info data)
    (lambda (key default)
      (case key
        [(configure-runtime)
         '(#(racket/private/runtime configure #f))]
        [else default]))))

