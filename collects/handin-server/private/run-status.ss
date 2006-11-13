(module run-status mzscheme
  (define current-run-status-box (make-parameter #f))

  (define (current-run-status s)
    (let ([b (current-run-status-box)])
      (when b (set-box! b s) (message s))))

  (define current-messenger (make-parameter #f))
  (define (message . args)
    (let ([messenger (current-messenger)])
      (and messenger (apply messenger args))))

  (provide current-run-status-box current-run-status
           current-messenger message))

