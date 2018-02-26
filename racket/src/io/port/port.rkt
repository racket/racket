#lang racket/base
(require "../host/thread.rkt"
         "evt.rkt")

(provide (struct-out core-port)
         (struct-out closed-state))

(struct core-port (name      ; anything, reported as `object-name` for the port
                   data      ; anything, effectively a subtype indicator

                   close     ; -> (void)
                   ;;          Called in atomic mode.

                   count-lines!  ; #f or procedure called in atomic mode
                   get-location  ; #f or procedure called in atomic mode
                   file-position ; #f, port, or procedure called in atomic mode
                   buffer-mode   ; #f or procedure in atomic mode

                   closed        ; `closed-state`

                   [offset #:mutable] ; count plain bytes
                   [count? #:mutable] ; whether line counting is enabled
                   [state #:mutable] ; state of UTF-8 decoding
                   [cr-state #:mutable] ; state of CRLF counting as a single LF
                   [line #:mutable]   ; count newlines
                   [column #:mutable] ; count UTF-8 characters in line
                   [position #:mutable]) ; count UTF-8 characters
  #:authentic
  #:property prop:object-name (struct-field-index name)
  #:property prop:secondary-evt port->evt)

(struct closed-state ([closed? #:mutable]
                      [closed-sema #:mutable]) ; #f or a semaphore posed on close
  #:authentic)
