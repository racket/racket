(module run-status mzscheme
  (define current-run-status-box (make-parameter #f))

  (define (current-run-status s)
    (let ([b (current-run-status-box)])
      (when b
	(set-box! b s))))

  (provide current-run-status-box
	   current-run-status))

