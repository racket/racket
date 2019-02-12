#lang racket/base
(require "../common/class.rkt"
         "../host/thread.rkt"
         "../host/pthread.rkt"
         "evt.rkt"
         "place-message.rkt")

(provide (struct-out core-port)
         (struct-out location)
         get-core-port-offset)

(class core-port
  (field
   [name 'port #:immutable] ; anything, reported as `object-name` for the port

   [data #f] ; FIXME: remove after all uses are converted

   ;; When `buffer` is #f, it enables a shortcut for reading and
   ;; writing, where `buffer-pos` must also be less than `buffer-end`
   ;; for the shortcut to apply. The shortcut is not necessarily
   ;; always taken, just if it is used, the `buffer-pos` position can
   ;; be adjusted and the port's methods must adapt accordingly. The
   ;; `buffer` and `buffer-end` fields are modified only by the port's
   ;; methods, however.
   ;;
   ;; For an input port, shortcut mode implies that `prepare-change`
   ;; does not need to be called, and no checking is needed for whether
   ;; the port is closed.
   ;;
   ;; A non-#f `buffer` further implies that `buffer-pos` should be
   ;; added to `offset` to get the true offset.
   [buffer #f]
   [buffer-pos 0]   ; if < `buffer-end`, allows direct read/write on `buffer`
   [buffer-end 0]
   
   [closed? #f]
   [closed-sema #f]
   
   [offset 0]   ; count plain bytes; add `(- buffer-pos buffer-start)`
   [count #f])  ; #f or a `location`
  
  ;; Various methods below are called in atomic mode. The intent of
  ;; atomic mode is to ensure that the completion and return of the
  ;; function is atomic with respect to some further activity, such
  ;; as position and line counting. Also, a guard against operations
  ;; on a closed port precedes most operations. Any of the functions
  ;; is free to exit and re-enter atomic mode, but they may take on
  ;; the burden of re-checking for a closed port. Leave atomic mode
  ;; explicitly before raising an exception.

  (public
   ;; -*> (void)
   ;; Called in atomic mode.
   ;; Reqeusts a close, and the port is closed if/when
   ;; the method returns.
   [close (lambda () (void))]

   ;; #f or (-*> (void))
   ;; Called in atomic mode.
   ;; Notifies the port that line counting is enabled, and
   ;; `get-location` can be called afterward (if it is defined)
   [count-lines! #f]

   ;; #f or (-*> (values line-or-#f column-or-#f position-or-#f))
   ;; Called in atomic mode.
   ;; Returns the location of the next character. If #f, this method
   ;; is implemented externally.
   [get-location #f]  ; #f or method called in atomic mode

   ;; #f or (U (-*> position-k) (position-k -*> (void))
   ;; Called in atomic mode.
   ;; If not #f, the port implements `file-position`.
   [file-position #f]

   ;; #f or (U (-*> mode-sym) (mode-sym -*> (void))
   ;; Called in atomic mode.
   ;; If not #f, the port implements buffer-mode selection.
   [buffer-mode #f])

  (property
   [prop:unsafe-authentic-override #t] ; allow evt chaperone
   [prop:object-name (struct-field-index name)]
   [prop:secondary-evt port->evt]))

(struct location ([state #:mutable]      ; state of UTF-8 decoding
                  [cr-state #:mutable]   ; state of CRLF counting as a single LF
                  [line #:mutable]       ; count newlines
                  [column #:mutable]     ; count UTF-8 characters in line
                  [position #:mutable])) ; count UTF-8 characters

(define (get-core-port-offset p)
  (define offset (core-port-offset p))
  (and offset
       (if (core-port-buffer p)
           (+ offset (core-port-buffer-pos p))
           offset)))
