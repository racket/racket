#lang racket/base
(require syntax/module-reader
         (only-in "../private/readtable.rkt" make-2d-readtable))

(provide (rename-out [2d-read read]
                     [2d-read-syntax read-syntax]
                     [2d-get-info get-info]))

(define (wrap-reader p)
  (lambda args
    (parameterize ([current-readtable (make-2d-readtable)])
      (apply p args))))

(define-values (2d-read 2d-read-syntax 2d-get-info)
  (make-meta-reader
   '2d
   "language path"
   (lambda (bstr)
     (let* ([str (bytes->string/latin-1 bstr)]
            [sym (string->symbol str)])
       (and (module-path? sym)
            (vector
             ;; try submod first:
             `(submod ,sym reader)
             ;; fall back to /lang/reader:
             (string->symbol (string-append str "/lang/reader"))))))
   wrap-reader
   wrap-reader
   (lambda (proc)
     (lambda (key defval)
       (case key
         [(color-lexer)
          (define theirs 
            (or (and proc (proc key #f))
                (dynamic-require 'syntax-color/racket-lexer 'racket-lexer)))
          ((dynamic-require 'unstable/2d/lexer '2d-lexer) theirs)]
         [else (if proc (proc key defval) defval)])))))
