
(module port scheme/base
  (require mzlib/port
           "private/portlines.ss")
  (provide (except-out (all-from-out mzlib/port)
                       strip-shell-command-start)
           port->string
           port->bytes
           port->lines
           port->bytes-lines
           display-lines)

  (define (port->string-port who p)
    (unless (input-port? p)
      (raise-type-error who "input-port" p))
    (let ([s (open-output-string)])
      (copy-port p s)
      s))

  (define (port->string [p (current-input-port)])
    (get-output-string (port->string-port 'port->string p)))

  (define (port->bytes [p (current-input-port)])
    (get-output-bytes (port->string-port 'port->bytes p) #t))

  (define (port->lines [p (current-input-port)] #:line-mode [mode 'any])
    (port->x-lines 'port->lines p mode read-line))
  
  (define (port->bytes-lines [p (current-input-port)] #:line-mode [mode 'any])
    (port->x-lines 'port->bytes-lines p mode read-bytes-line))

  (define (display-lines l [p (current-output-port)] #:separator [newline #"\n"])
    (unless (list? l)
      (raise-type-error 'display-lines "list" l))
    (unless (output-port? p)
      (raise-type-error 'display-lines "output-port" p))
    (do-lines->port l p newline)))
