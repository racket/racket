#lang racket/base

(require mzlib/port
         "private/portlines.rkt")
(provide (except-out (all-from-out mzlib/port)
                     strip-shell-command-start)
         port->string
         port->bytes
         port->lines
         port->bytes-lines
         port->list
         display-lines

         with-output-to-string
         with-output-to-bytes
         call-with-output-string
         call-with-output-bytes

         with-input-from-string
         with-input-from-bytes
         call-with-input-string
         call-with-input-bytes)

(define (port->string-port who p)
  (unless (input-port? p) (raise-type-error who "input-port" p))
  (let ([s (open-output-string)]) (copy-port p s) s))

(define (port->string [p (current-input-port)])
  (get-output-string (port->string-port 'port->string p)))

(define (port->bytes [p (current-input-port)])
  (get-output-bytes (port->string-port 'port->bytes p) #t))

(define (port->lines [p (current-input-port)] #:line-mode [mode 'any])
  (port->x-lines 'port->lines p mode read-line))

(define (port->bytes-lines [p (current-input-port)] #:line-mode [mode 'any])
  (port->x-lines 'port->bytes-lines p mode read-bytes-line))

(define (port->list [r read] [p (current-input-port)])
  (unless (input-port? p)
    (raise-type-error 'port->list "input-port" p))
  (unless (and (procedure? r) (procedure-arity-includes? r 1))
    (raise-type-error 'port->list "procedure (arity 1)" r))
  (for/list ([v (in-port r p)]) v))

(define (display-lines l [p (current-output-port)] #:separator [newline #"\n"])
  (unless (list? l) (raise-type-error 'display-lines "list" l))
  (unless (output-port? p) (raise-type-error 'display-lines "output-port" p))
  (do-lines->port l p newline))

(define (with-output-to-x who n proc)
  (unless (and (procedure? proc) (procedure-arity-includes? proc n))
    (raise-type-error who (format "procedure (arity ~a)" n) proc))
  (let ([s (open-output-bytes)])
    ;; Use `dup-output-port' to hide string-port-ness of s:
    (if (zero? n)
      (parameterize ([current-output-port (dup-output-port s #t)])
        (proc))
      (proc (dup-output-port s #t)))
    s))

(define (with-output-to-string proc)
  (get-output-string (with-output-to-x 'with-output-to-string 0 proc)))

(define (with-output-to-bytes proc)
  (get-output-bytes (with-output-to-x 'with-output-to-bytes 0 proc) #t))

(define (call-with-output-string proc)
  (get-output-string (with-output-to-x 'call-with-output-string 1 proc)))

(define (call-with-output-bytes proc)
  (get-output-bytes (with-output-to-x 'call-with-output-bytes 1 proc) #t))

(define (with-input-from-x who n b? str proc)
  (unless (if b? (bytes? str) (string? str))
    (raise-type-error who (if b? "byte string" "string") 0 str proc))
  (unless (and (procedure? proc) (procedure-arity-includes? proc n))
    (raise-type-error who (format "procedure (arity ~a)" n) 1 str proc))
  (let ([s (if b? (open-input-bytes str) (open-input-string str))])
    (if (zero? n)
      (parameterize ([current-input-port s])
        (proc))
      (proc s))))

(define (with-input-from-string str proc)
  (with-input-from-x 'with-input-from-string 0 #f str proc))

(define (with-input-from-bytes str proc)
  (with-input-from-x 'with-input-from-bytes 0 #t str proc))

(define (call-with-input-string str proc)
  (with-input-from-x 'call-with-input-string 1 #f str proc))

(define (call-with-input-bytes str proc)
  (with-input-from-x 'call-with-input-bytes 1 #t str proc))
