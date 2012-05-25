#lang racket/base

(provide port->x-lines
         check-mode
         do-lines->port)

(define (check-mode who mode)
  (unless (memq mode '(linefeed return return-linefeed any any-one))
    (raise-argument-error who "(or/c 'linefeed 'return 'return-linefeed 'any 'any-one)" mode)))

(define (port->x-lines who p mode read-line)
  (unless (input-port? p)
    (raise-argument-error who "input-port?" p))
  (check-mode who mode)
  (let loop ([l null])
    (let ([line (read-line p mode)])
      (if (eof-object? line)
          (reverse l)
          (loop (cons line l))))))

(define (do-lines->port l p newline)
  (for ([i (in-list l)])
    (display i p)
    (display newline p)))
