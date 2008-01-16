;; This is a wrapper around "rep-start.ss" -- use it if we're using a terminal
#lang scheme/base

(require scheme/runtime-path
         (for-syntax scheme/base))

(define-runtime-path rep-start "rep-start.ss")

(provide install-readline!)

(let ([inp (current-input-port)] [outp (current-output-port)])
  (when (and (eq? 'stdin (object-name inp)) (terminal-port? inp))
    (dynamic-require rep-start #f)
    (when (terminal-port? outp)
      (port-count-lines! outp))))

(define readline-init-expr
  '(require readline/rep))

(define (install-readline!)
  (let ([file (find-system-path 'init-file)])
    (when (or (not (file-exists? file))
              (not (with-handlers ([exn:fail? 
                                    (lambda (exn)
                                      (error 'install-readline!
                                             "trouble reading existing ~e: ~a"
                                             file
                                             (exn-message exn)))])
                     (call-with-input-file*
                      file
                      (lambda (in)
                        (let loop ()
                          (let ([v (read in)])
                            (cond
                             [(eof-object? v) #f]
                             [(equal? v readline-init-expr) #t]
                             [else (loop)]))))))))
      (call-with-output-file*
       file
       #:exists 'append
       (lambda (out)
         (newline out)
         (write readline-init-expr out)
         (newline out))))))
