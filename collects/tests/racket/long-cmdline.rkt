#lang racket/base
(require racket/system)

(let ([p (find-executable-path (find-system-path 'exec-file))])
  (let ([s (open-output-bytes)])
    (parameterize ([current-output-port s])
      (apply system* p "-e" "'done" (for/list ([i 8192]) "x")))
    (unless (equal? (get-output-bytes s) #"'done\n")
      (error "test failed"))))
