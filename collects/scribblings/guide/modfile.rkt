#lang racket/base

(require scribble/manual
         (for-syntax racket/base
                     (prefix-in c: scribble/comment-reader)
                     syntax/strip-context
                     compiler/cm-accomplice))

(provide racketmodfile)

(define-syntax (racketmodfile stx)
  (syntax-case stx ()
    [(_ file)
     (let ([f (path->complete-path (syntax-e #'file)
                                   (or (current-load-relative-directory)
                                       (current-directory)))])
       (register-external-file f)
       (with-syntax ([(content ...)
                      (call-with-input-file* 
                       f
                       (lambda (in)
                         (read-bytes 6 in)
                         (port-count-lines! in)
                         (let loop ()
                           (let ([v (c:read-syntax (object-name in) in)])
                             (if (eof-object? v) 
                                 null 
                                 (cons (replace-context #'file v) 
                                       (loop)))))))])
         #'(racketmod #:file file content ...)))]))
