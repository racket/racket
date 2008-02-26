#lang scheme/base

(require rnrs/enums-6
         rnrs/conditions-6
         r6rs/private/io-conds
         scheme/port)

(provide (all-from-out r6rs/private/io-conds)
         file-options
         buffer-mode buffer-mode?
         latin-1-codec utf-8-codec utf-16-codec
         eol-style native-eol-style
         &i/o-decoding make-i/o-decoding-error i/o-decoding-error?
         &i/o-encoding make-i/o-encoding-error i/o-encoding-error? i/o-encoding-error-char
         error-handling-mode
         (rename-out [r6rs:make-transcoder make-transcoder])
         transcoder-codec
         transcoder-eol-style
         transcoder-error-handling-mode
         ;bytevector->string
         ;string->bytevector
         (rename-out [eof eof-object])
         eof-object?
         port?
         port-transcoder
         textual-port?
         binary-port?
         transcoded-port
         port-has-port-position?
         port-position
         port-has-set-port-position!?
         set-port-position!
         close-port
         call-with-port
         input-port?
         port-eof?
         open-file-input-port
         open-bytevector-input-port
         open-string-input-port
         standard-input-port
         current-input-port
         make-custom-binary-input-port
         make-custom-textual-input-port)

;; ----------------------------------------

(define-enumeration -file-option (no-create no-fail no-truncate)
  file-options)

(define-enumeration buffer-mode (none line block)
  -buffer-modes)

(define (buffer-mode? m)
  (enum-set-member? m (-buffer-modes none line block)))

(define-enumeration eol-style (lf cr crlf nel crnel ls)
  -eol-styles)

(define-struct codec (enc))
(define latin-1 (make-codec "latin-1"))
(define utf-8 (make-codec "utf-8"))
(define utf-16 (make-codec "utf-16"))

(define (latin-1-codec) latin-1)
(define (utf-8-codec) utf-8)
(define (utf-16-codec) utf-16)

(define (native-eol-style)
  (if (eq? (system-type) 'windows)
      'crlf
      'lf))

(define-condition-type &i/o-decoding &i/o-port
  make-i/o-decoding-error i/o-decoding-error?)

(define-condition-type &i/o-encoding &i/o-port
  make-i/o-encoding-error i/o-encoding-error?
  (char i/o-encoding-error-char))

(define-enumeration error-handling-mode (ignore raise replace)
  -handling-modes)

(define-struct transcoder (codec eol-style error-handling-mode))

(define (r6rs:make-transcoder codec 
                              [eol-style (native-eol-style)]
                              [handling-mode 'replace])
  (unless (codec? codec)
    (raise-type-error 'make-transcoder "codec" codec))
  (unless (enum-set-member? eol-style (-eol-styles lf cr crlf nel crnel ls))
    (raise-type-error 'make-transcoder "'lf, 'cr, 'crlf, 'nel, 'crnel, or 'ls" eol-style))
  (unless (enum-set-member? handling-mode (-handling-modes ignore raise replace))
    (raise-type-error 'make-transcoder "'ignore, 'raise, or 'replace" eol-style))
  (make-transcoder codec eol-style handling-mode))

(define (native-transcoder)
  (make-transcoder utf-8))

;; ----------------------------------------

(define (make-disconnectable-input-port port)
  (define disconnected? #f)
  (define (check-disconnect)
    (when disconnected?
      (error 'read-byte "cannot read for transcoded binary port")))
  (values
   (make-input-port
    (object-name port)
    (lambda (bytes)
      (check-disconnect)
      (let ([n (read-bytes-avail!* bytes port)])
        (if (eq? n 0)
            port
            n)))
    (lambda (bytes skip evt)
      (check-disconnect)
      (let ([n (peek-bytes-avail! bytes skip evt port)])
        (if (eq? n 0)
            port
            n)))
    (lambda ()
      (unless disconnected?
        (close-input-port port)))
    (and (port-provides-progress-evts? port)
         (lambda ()
           (check-disconnect)
           (port-progress-evt port)))
    (and (port-provides-progress-evts? port)
         (lambda (k evt done)
           (port-commit-peeked k evt done port)))
    (lambda ()
      (check-disconnect)
      (port-next-location port))
    (lambda ()
      (check-disconnect)
      (port-count-lines! port))
    1
    (case-lambda
     [() (check-disconnect) (file-stream-buffer-mode port)]
     [(mode) (check-disconnect) (file-stream-buffer-mode port mode)]))
   (lambda ()
     (set! disconnected? #t)
     port)))

(define (make-disconnectable-output-port port)
  (define disconnected? #f)
  (define (check-disconnect)
    (when disconnected?
      (error 'read-byte "cannot read for transcoded binary port")))
  (values
   (make-output-port
    (object-name port)
    port
    (lambda (bytes start end can-buffer/block? enable-breaks?)
      (check-disconnect)
      (cond
       [enable-breaks?
        (parameterize-break #t (write-bytes (subbytes start end) port))]
       [can-buffer/block?
        (write-bytes (subbytes start end) port)]
       [else
        (write-bytes-avail* (subbytes start end) port)]))
    (lambda ()
      (unless disconnected?
        (close-output-port port)))
    (and (port-writes-special? port)
         (lambda (v can-buffer/block? enable-breaks?)
           (check-disconnect)
           (cond
            [enable-breaks?
             (parameterize-break #t (write-special v port))]
            [can-buffer/block?
             (write-special v port)]
            [else
             (write-special-avail* v port)])))
    (and (port-writes-atomic? port)
         (lambda (bytes start end)
           (check-disconnect)
           (write-bytes-avail-evt bytes port start end)))
    (and (port-writes-special? port)
         (port-writes-atomic? port)
         (lambda (v)
           (check-disconnect)
           (write-special-evt v port)))
    (lambda ()
      (check-disconnect)
      (port-next-location port))
    (lambda ()
      (check-disconnect)
      (port-count-lines! port))
    1
    (case-lambda
     [() (check-disconnect) (file-stream-buffer-mode port)]
     [(mode) (check-disconnect) (file-stream-buffer-mode port mode)]))
   (lambda ()
     (set! disconnected? #t)
     port)))

;; R6RS functions that generate binary ports wrap them with `binary-...-port'
;; structures, so that the binary ports can be "closed" by `transcoded-port'.
(define-struct binary-input-port (port disconnect get-pos set-pos!)
  #:property prop:input-port 0)
(define-struct binary-output-port (port disconnect get-pos set-pos!)
  #:property prop:output-port 0)
(define-struct (binary-input/output-port binary-input-port) (out-port out-disconnect)
  #:property prop:output-port 0)

;; Textual ports are transcoded
(define-struct textual-input-port (port transcoder)
  #:property prop:input-port 0)
(define-struct textual-output-port (port  transcoder)
  #:property prop:output-port 0)
(define-struct (textual-input/output-port textual-input-port) (out-port)
  #:property prop:output-port 0)

(define (port-transcoder port)
  (cond
   [(textual-input-port? port) (textual-input-port-transcoder port)]
   [(textual-output-port? port) (textual-output-port-transcoder port)]
   [(input-port? port) #f]
   [(output-port? port) #f]
   [else (raise-type-error 'port-transcoder "port" port)]))

(define (textual-port? v)
  (if (port? v)
      (or (textual-input-port? v)
          (textual-output-port? v))
      (raise-type-error 'textual-port? "port" v)))

(define (binary-port? v)
  (if (port? v)
      (not (or (textual-input-port? v)
               (textual-output-port? v)))
      (raise-type-error 'binary-port? "port" v)))

(define (wrap-binary-input-port p get-pos set-pos!)
  (let-values ([(p disconnect) (make-disconnectable-input-port p)])
    (make-binary-input-port p disconnect get-pos set-pos!)))

(define (transcode-input p t)
  (let ([p (if (binary-input-port? p)
               ((binary-input-port-disconnect p))
               p)])
    (reencode-input-port p 
                         (codec-enc (transcoder-codec t))
                         (case (transcoder-error-handling-mode t)
                           [(raise) #f]
                           [(ignore) #""]
                           [(replace) (string->bytes/utf-8 "\uFFFD")])
                         #t)))

(define (transcode-output p t)
  (let ([p (cond
            [(binary-output-port? p)
             ((binary-output-port-disconnect p))]
            [(binary-input/output-port? p)
             ((binary-input/output-port-out-disconnect p))]
            [else p])])
    (reencode-output-port p 
                          (codec-enc (transcoder-codec t))
                          (case (transcoder-error-handling-mode t)
                            [(raise) #f]
                            [(ignore) #""]
                            [(replace) (string->bytes/utf-8 "\uFFFD")])
                          #t)))

(define (transcoded-port p t)
  (unless (and (port? p)
               (binary-port? p))
    (raise-type-error 'transcoded-port "binary port" p))
  (unless (transcoder? t)
    (raise-type-error 'transcoded-port "transcoder" t))
  (cond
   [(and (input-port? p) (output-port? p))
    (make-textual-input/output-port (transcode-input p)
                                    t 
                                    (transcode-output p))]
   [(input-port? p)
    (make-textual-input-port (transcode-input p t) t)]
   [(output-port? p)
    (make-textual-input-port (transcode-output p t) t)]))

(define (port-has-port-position? p)
  (unless (port? p)
    (raise-type-error 'port-has-port-position? "port" p))
  (cond
   [(binary-input-port? p)
    (and (binary-input-port-get-pos p))]
   [(binary-output-port? p)
    (and (binary-output-port-get-pos p))]
   [(textual-input-port? p)
    (port-has-port-position? (textual-input-port-port p))]
   [(textual-output-port? p)
    (port-has-port-position? (textual-output-port-port p))]
   [else #t]))

(define (port-position p)
  (cond 
   [(binary-input-port? p)
    ((binary-input-port-get-pos p))]
   [(binary-output-port? p)
    ((binary-output-port-get-pos p))]
   [(textual-input-port? p)
    (port-position (textual-input-port-port p))]
   [(textual-output-port? p)
    (port-position (textual-output-port-port p))]
   [else (file-position p)]))

(define (port-has-set-port-position!? p)
  (unless (port? p)
    (raise-type-error 'port-has-port-set-position!? "port" p))
  (cond 
   [(binary-input-port? p)
    (and (binary-input-port-set-pos! p) #t)]
   [(binary-output-port? p)
    (and (binary-output-port-set-pos! p) #t)]
   [(textual-input-port? p)
    (port-has-set-port-position!? (textual-input-port-port p))]
   [(textual-output-port? p)
    (port-has-set-port-position!? (textual-output-port-port p))]
   [else
    ;; FIXME
    (or (file-stream-port? p)
        #t)]))

(define (set-port-position! p pos)
  (unless (and (port? p)
               (port-has-set-port-position!? p))
    (raise-type-error 'set-port-position! "port with settable position" p))
  (cond 
   [(binary-input-port? p)
    ((binary-input-port-set-pos! p) pos)]
   [(binary-output-port? p)
    ((binary-output-port-set-pos! p) pos)]
   [(textual-input-port? p)
    (set-port-position! (textual-input-port-port p) pos)]
   [(textual-output-port? p)
    (set-port-position! (textual-output-port-port p) pos)]
   [else
    (file-position p pos)]))

(define (call-with-port port proc)
  (unless (port? port)
    (raise-type-error 'call-with-port "port" port))
  (begin0
   (proc port)
   (close-port port)))

(define (close-port port)
  (when (input-port? port)
    (close-input-port port))
  (when (output-port? port)
    (close-output-port port)))

;; ----------------------------------------

(define (port-eof? p)
  (eof-object? (peek-byte p)))

(define (open-file-input-port filename 
                              [options (file-options)]
                              [buffer-mode 'block]
                              [maybe-transcoder #f])
  (unless (enum-set=? (enum-set-universe options)
                      (enum-set-universe (file-options)))
    (raise-type-error 'open-file-input-port "file-options enum set" options))
  (unless (enum-set-member? buffer-mode (-buffer-modes none line block))
    (raise-type-error 'open-file-input-port "'none, 'line, or 'block" buffer-mode))
  (when maybe-transcoder
    (unless (transcoder? maybe-transcoder)
      (raise-type-error 'open-file-input-port "transcoder or #f" maybe-transcoder)))
  (let ([p (open-input-file filename)])
    (file-stream-buffer-mode p buffer-mode)
    (if maybe-transcoder
        (transcoded-port p maybe-transcoder)
        (wrap-binary-input-port p 
                                (lambda () (file-position p)) 
                                (lambda (pos) (file-position p pos))))))

(define (open-bytevector-input-port bytes [maybe-transcoder #f])
  (unless (bytes? bytes)
    (raise-type-error 'open-bytevector-input-port "bytevector" bytes))
  (when maybe-transcoder
    (unless (transcoder? maybe-transcoder)
      (raise-type-error 'open-bytevector-input-port "transcoder or #f" maybe-transcoder)))
  (let ([p (open-input-bytes bytes)])
    (if maybe-transcoder
        (transcoded-port p maybe-transcoder)
        (wrap-binary-input-port p
                                (lambda () (file-position p)) 
                                (lambda (pos) (file-position p pos))))))

(define (open-string-input-port str)
  (unless (string? str)
    (raise-type-error 'open-bytevector-input-port "string" str))
  (transcoded-port (open-input-string str) utf-8))

(define standard-input-port
  (let ([p (current-input-port)])
    (wrap-binary-input-port p
                            (lambda () (file-position p)) 
                            (lambda (pos) (file-position p pos)))))

(define input-ports (make-hash-table 'weak))

(define (r6rs:current-input-port)
  (let ([p (current-input-port)])
    (cond
     [(textual-port? p) p]
     [(hash-table-get input-ports p #f)
      => ephemeron-value]
     [else
      (let ([p2 (transcoded-port p utf-8)])
        (hash-table-put! input-ports p (make-ephemeron p p2))
        p2)])))

(define (make-custom-binary-input-port id read! get-position set-position! close)
  (let ([p (make-input-port/read-to-peek
            id
            (lambda (bytes)
              (let ([v (read! bytes 0 (bytes-length bytes))])
                (if (zero? v)
                    eof
                    v)))
            #f
            close)])
    (wrap-binary-input-port p
                            get-position
                            set-position!)))


(define (make-custom-textual-input-port id read! get-position set-position! close)
  (make-textual-input-port
   (make-custom-binary-input-port
    id 
    (let-values ([(in out) (make-pipe)])
      (lambda (bstr offset len)
        (let loop ()
          (let ([n (read-bytes-avail! bstr in offset len)])
               (if (zero? n)
                   (let ([str (make-string (bytes-length bstr))])
                     (let ([len (read! str 0 (bytes-length bstr))])
                       (if (zero? len)
                           eof
                           (begin
                             (write-string (substring str 0 len) out)
                             (loop)))))
                   n)))))
    get-position
    set-position!
    close)))

;; ----------------------------------------
