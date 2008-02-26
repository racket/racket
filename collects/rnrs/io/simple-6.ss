#lang scheme/base

(require r6rs/private/readtable)

(provide (rename-out [eof eof-object])
         eof-object?
         call-with-input-file
         call-with-output-file
         input-port?
         output-port?
         current-input-port
         current-output-port
         current-error-port
         with-input-from-file
         with-output-to-file
         open-input-file
         open-output-file
         close-input-port
         close-output-port
         read-char
         peek-char
         (rename-out [r6rs:read read])
         write-char
         newline
         display
         write)

(define (r6rs:read [in (current-input-port)])
  (let loop ([v (with-r6rs-reader-parameters (lambda () (read in)))])
    (cond
     [(pair? v) (mcons (loop (car v))
                       (loop (cdr v)))]
     [(vector? v) (list->vector
                   (map loop (vector->list v)))]
     [else v])))
