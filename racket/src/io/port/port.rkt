#lang racket/base
(require "../common/class.rkt"
         "../host/thread.rkt"
         "../host/pthread.rkt"
         "evt.rkt")

(provide (struct-out core-port)
         (struct-out direct)
         (struct-out location)
         get-core-port-offset)

;; Port class hierarchy
;;    - with "virtual" in square brackets
;;    - with per-instance in curly braces
;;
;;                         [core]
;;                           |
;;            ,------------------------------,
;;         [input]                        [output]
;;            |                              |
;;       ,---------,             ,---------------------------------,
;;       |         |             |       |       |     |           |
;;    [commit]  {custom}      {custom}  pipe   bytes  fd   max   nowhere
;;       |     (when peek                              |
;;       |       provided)                            tcp
;;    -------------------------,
;;   '             |           |    
;;  bytes   [peek-via-read]  pipe
;;                 |
;;             ,-------,
;;             |       |
;;            fd    {custom}
;;             |    (when no peek provided)
;;            tcp

(class core-port
  #:field
  [name 'port #:immutable] ; anything, reported as `object-name` for the port

  [lock #f] ; keep this as the 2nd field; see "lock.rkt" for possible values

  ;; When `(direct-bstr buffer)` is not #f, it enables a shortcut for
  ;; reading and writing, where `(direct-pos buffer)` must also be
  ;; less than `(direct-end buffer)` for the shortcut to apply. The
  ;; shortcut is not necessarily always taken, but if it is used, the
  ;; `(direct-pos buffer)` position can be adjusted and the port's
  ;; methods must adapt accordingly. The `(direct-bstr buffer)` and
  ;; `(direct-end buffer)` fields are modified only by the port's
  ;; methods, however.
  ;;
  ;; Shortcut mode implies that the port is still open, so no checking
  ;; is needed for whether the port is closed.
  ;;
  ;; For an input port, shortcut mode implies that `prepare-change`
  ;; does not need to be called.
  ;;
  ;; A non-#f `(direct-bstr buffer)` further implies that
  ;; `(direct-pos buffer)` should be added to `offset` to get the
  ;; true offset.
  [buffer (direct #f 0 0)]
  
  [closed? #f]
  [closed-sema #f] ; created on demand
  
  [offset 0]   ; count plain bytes; add `(- buffer-pos buffer-start)`
  [count #f]   ; #f or a `location`

  ;; Various methods below are called in atomic mode. The intent of
  ;; atomic mode is to ensure that the completion and return of the
  ;; function is atomic with respect to some further activity, such
  ;; as position and line counting. Also, a guard against operations
  ;; on a closed port precedes most operations. Any of the functions
  ;; is free to exit and re-enter atomic mode, but they may take on
  ;; the burden of re-checking for a closed port. Leave atomic mode
  ;; explicitly before raising an exception.

  #:public
  ;; -*> (void)
  ;; Called with lock held.
  ;; Reqeusts a close, and the port is closed if/when
  ;; the method returns. May exit and reenter lock, if promoting
  ;; to atomic is necessary, only before doing anything interesting,
  ;; and that case it must perform it's own "already closed?"
  ;; check. The `close-port` implementation relies on the lock
  ;; being held after an internal close operation so that the `closed?`
  ;; flag can be set on the port.
  [close (lambda () (void))]

  ;; #f or (-*> (void))
  ;; Called with lock held.
  ;; Notifies the port that line counting is enabled, and
  ;; `get-location` can be called afterward (if it is defined)
  [count-lines! #f]

  ;; #f or (-*> (values line-or-#f column-or-#f position-or-#f))
  ;; Called with lock held.
  ;; Returns the location of the next character. If #f, this method
  ;; is implemented externally.
  [get-location #f]  ; #f or method called in atomic mode

  ;; #f or (U (-*> position-k) (position-k -*> (void))
  ;; Called with lock held.
  ;; If not #f, the port implements `file-position`.
  [file-position #f]

  ;; #f or (U (-*> mode-sym) (mode-sym -*> (void))
  ;; Called with lock held.
  ;; If not #f, the port implements buffer-mode selection.
  [buffer-mode #f]

  #:property
  [prop:unsafe-authentic-override #t] ; allow evt chaperone
  [prop:object-name (struct-field-index name)]
  [prop:secondary-evt port->evt])

(struct direct ([bstr #:mutable]
                [pos #:mutable]
                [end #:mutable])
  #:authentic)

(struct location ([state #:mutable]      ; state of UTF-8 decoding
                  [cr-state #:mutable]   ; state of CRLF counting as a single LF
                  [line #:mutable]       ; count newlines
                  [column #:mutable]     ; count UTF-8 characters in line
                  [position #:mutable])  ; count UTF-8 characters
  #:authentic)

;; with p lock
(define (get-core-port-offset p)
  (define offset (core-port-offset p))
  (define buffer (core-port-buffer p))
  (and offset
       (if (direct-bstr buffer)
           (+ offset (direct-pos buffer))
           offset)))
