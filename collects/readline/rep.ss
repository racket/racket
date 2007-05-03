;; This is a wrapper around "rep-start.ss" -- use it if we're using a terminal
(module rep mzscheme
  (require (lib "runtime-path.ss"))
  (define-runtime-path rep-start "rep-start.ss")

  (let ([inp (current-input-port)] [outp (current-output-port)])
    (when (and (eq? 'stdin (object-name inp)) (terminal-port? inp))
      (dynamic-require rep-start #f)
      (when (terminal-port? outp)
        (port-count-lines! outp)))))
