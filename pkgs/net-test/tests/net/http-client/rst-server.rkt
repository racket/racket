#lang racket/base

;; Spawns a one-shot HTTP server that sends a RST packet in the middle
;; of sending its response content. Relies on the availability of a C
;; compiler in order to determine the values of some C socket constants.

(require ffi/unsafe
         ffi/unsafe/port
         racket/runtime-path
         racket/system
         racket/tcp)

(define cc (find-executable-path "cc"))
(define-runtime-path rst-server-constants
  "rst-server-constants")
(define-runtime-path rst-server-constants.c
  "rst-server-constants.c")

;; Returns the values of SOL_SOCKET and SO_LINGER for the current
;; platform (POSIX only).
(define (get-constants)
  (parameterize ([current-subprocess-custodian-mode 'kill]
                 [subprocess-group-enabled #t])
    (unless (zero? (system*/exit-code cc "-o" rst-server-constants rst-server-constants.c))
      (error 'get-constants "failed to compile"))
    (define out (open-output-string))
    (parameterize ([current-output-port out])
      (unless (zero? (system*/exit-code rst-server-constants))
        (error 'get-constants "failed to get constants")))
    (parameterize ([current-input-port (open-input-string (get-output-string out))])
      (values (read) (read)))))

(define-cstruct _linger
  ([l_onoff _int]
   [l_linger _int]))

(define setsockopt
  (get-ffi-obj "setsockopt" #f (_fun _int _int _int _linger-pointer _size -> _int)))

(define (serve mode)
  (define-values (SOL_SOCKET SO_LINGER)
    (get-constants))
  (define listener (tcp-listen 0 512 #t))
  (define-values (_local-host local-port _remote-host _remote-port)
    (tcp-addresses listener #t))
  (define accept-thd
    (thread
     (lambda ()
       (define-values (in out)
         (tcp-accept listener))
       (define sock (unsafe-port->socket out))
       (define opt (make-linger 1 0))
       (setsockopt sock SOL_SOCKET SO_LINGER opt (ctype-sizeof _linger))
       (for ([line (in-lines in)])
         #:break (equal? line "")
         (void))
       (close-input-port in)
       (fprintf out "HTTP/1.1 200 OK\r\n")
       (case mode
         [(chunked)
          (fprintf out "Transfer-Encoding: chunked\r\n")
          (fprintf out "\r\n")
          (fprintf out "2\r\n")
          (fprintf out "hi\r\n")
          (fprintf out "50\r\n")]
         [(full)
          (fprintf out "Content-Length: 50\r\n")
          (fprintf out "\r\n")
          (fprintf out "hello")])
       ;; On Linux, the socket needs to be in a half-closed state for
       ;; the RST to get sent reliably.
       (tcp-abandon-port out)
       (close-output-port out))))
  (values
   local-port
   (thread-dead-evt accept-thd)
   (λ () (tcp-close listener))))

(module+ main
  (require racket/cmdline)
  (define mode
    (command-line
     #:args [MODE]
     (case MODE
       [("chunked") 'chunked]
       [("full") 'full]
       [else (error "MODE must be either 'chunked' or 'full'")])))
  (file-stream-buffer-mode (current-output-port) 'line)
  (file-stream-buffer-mode (current-error-port) 'line)
  (define-values (port done?-evt stop)
    (serve mode))
  (printf "PORT: ~a~n" port)
  (with-handlers ([exn:break? void])
    (void (sync/enable-break done?-evt)))
  (stop))
