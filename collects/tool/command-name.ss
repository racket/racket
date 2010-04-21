#lang scheme/base

(provide current-command-name
         program+command-name
         short-program+command-name)

(define current-command-name (make-parameter #f))

(define (program+command-name)
  (let ([p (find-system-path 'run-file)]
        [n (current-command-name)])
    (if n
        (format "~a ~a" p n)
        p)))

(define (short-program+command-name)
  (let ([p (find-system-path 'run-file)]
        [n (current-command-name)])
    (let-values ([(base name dir?) (split-path p)])
      (if n
          (format "~a ~a" name n)
          (path->string name)))))

