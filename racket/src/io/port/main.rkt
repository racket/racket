#lang racket/base
(require (only-in "input-port.rkt"
                  input-port?
                  prop:input-port)
         (only-in "output-port.rkt"
                  output-port?
                  prop:output-port)
         "bytes-input.rkt"
         "string-input.rkt"
         "special-input.rkt"
         "progress-evt.rkt"
         "bytes-output.rkt"
         "string-output.rkt"
         "special-output.rkt"
         "line-input.rkt"
         "file-port.rkt"
         "file-stream.rkt"
         (only-in "fd-port.rkt"
                  terminal-port?)
         "file-identity.rkt"
         "file-lock.rkt"
         "bytes-port.rkt"
         "string-port.rkt"
         "custom-input-port.rkt"
         "custom-output-port.rkt"
         "handler.rkt"
         "pipe.rkt"
         "close.rkt"
         "count.rkt"
         "buffer-mode.rkt"
         "file-position.rkt"
         "file-truncate.rkt"
         "flush-output.rkt"
         "parameter.rkt"
         "ready.rkt")

(provide read-byte
         read-bytes
         read-bytes!
         read-bytes-avail!
         read-bytes-avail!*
         read-bytes-avail!/enable-break
         
         peek-byte
         peek-bytes
         peek-bytes!
         peek-bytes-avail!
         peek-bytes-avail!*
         peek-bytes-avail!/enable-break

         read-byte-or-special
         peek-byte-or-special
         read-char-or-special
         peek-char-or-special

         port-provides-progress-evts?
         progress-evt?
         port-progress-evt
         port-commit-peeked

         read-char
         read-string
         read-string!
         
         peek-char
         peek-string
         peek-string!

         byte-ready?
         char-ready?

         write-byte
         write-bytes
         write-bytes-avail
         write-bytes-avail*
         write-bytes-avail/enable-break
         write-bytes-avail-evt
         write-char
         write-string
         port-writes-atomic?

         write-special
         write-special-avail*
         write-special-evt
         port-writes-special?
         
         read-line
         read-bytes-line
         
         make-input-port
         make-output-port

         port-read-handler
         port-write-handler
         port-display-handler
         port-print-handler
         install-reader!
         global-port-print-handler

         prop:input-port
         prop:output-port
         input-port?
         output-port?
         
         open-input-file
         open-output-file
         open-input-output-file
         call-with-input-file
         call-with-output-file
         with-input-from-file
         with-output-to-file

         file-stream-port?
         terminal-port?

         open-input-bytes
         open-output-bytes
         get-output-bytes
         open-input-string
         open-output-string
         get-output-string
         string-port?
         
         make-pipe
         pipe-input-port?
         pipe-output-port?
         pipe-content-length

         port-closed?
         close-input-port
         close-output-port
         port-closed-evt

         file-stream-buffer-mode

         port-file-identity
         port-try-file-lock?
         port-file-unlock

         file-position
         file-position*
         file-truncate
         
         port-count-lines!
         port-counts-lines?
         port-next-location
         set-port-next-location!
         port-count-lines-enabled

         current-input-port
         current-output-port
         current-error-port

         flush-output)
