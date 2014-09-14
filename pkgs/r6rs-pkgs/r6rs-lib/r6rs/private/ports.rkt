#lang scheme/base

;; FIXME: newline decoding

(require rnrs/enums-6
         rnrs/conditions-6
         r6rs/private/io-conds
         r6rs/private/readtable
         r6rs/private/exns
         scheme/port
         scheme/pretty
         scheme/promise)

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
         native-transcoder
         bytevector->string
         string->bytevector
         eof-object
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
         (rename-out [r6rs:current-input-port current-input-port])
         make-custom-binary-input-port
         make-custom-textual-input-port
         get-u8
         lookahead-u8
         get-bytevector-n
         get-bytevector-n!
         get-bytevector-some
         get-bytevector-all
         get-char
         lookahead-char
         get-string-n
         get-string-n!
         get-string-all
         get-line
         get-datum
         output-port?
         flush-output-port
         output-port-buffer-mode
         open-file-output-port
         open-bytevector-output-port
         call-with-bytevector-output-port
         open-string-output-port
         call-with-string-output-port
         standard-output-port
         standard-error-port
         (rename-out [r6rs:current-output-port current-output-port]
                     [r6rs:current-error-port current-error-port])
         make-custom-binary-output-port
         make-custom-textual-output-port
         put-u8
         put-bytevector
         put-char
         put-string
         put-datum
         open-file-input/output-port
         make-custom-binary-input/output-port
         make-custom-textual-input/output-port

         ;; Non-standard
         r6rs-port->port)

;; ----------------------------------------

(define-enumeration -file-option (no-create no-fail no-truncate)
  file-options)

(define-enumeration buffer-mode (none line block)
  -buffer-modes)

(define (buffer-mode? m)
  (enum-set-member? m (-buffer-modes none line block)))

(define-enumeration eol-style (lf cr crlf nel crnel ls none)
  -eol-styles)

(define-struct codec (enc))
(define latin-1 (make-codec "latin1"))
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
  (unless (enum-set-member? eol-style (-eol-styles lf cr crlf nel crnel ls none))
    (raise-type-error 'make-transcoder "'lf, 'cr, 'crlf, 'nel, 'crnel, 'ls, or 'none" eol-style))
  (unless (enum-set-member? handling-mode (-handling-modes ignore raise replace))
    (raise-type-error 'make-transcoder "'ignore, 'raise, or 'replace" eol-style))
  (make-transcoder codec eol-style handling-mode))

(define utf8-transcoder 
  (make-transcoder utf-8 'none '?))

(define (native-transcoder)
  utf8-transcoder)

(define (eof-object) eof)

;; ----------------------------------------

(define (make-disconnectable-input-port port close?)
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
            (wrap-evt port (lambda (v) 0))
            n)))
    (lambda (bytes skip evt)
      (check-disconnect)
      (let ([n (peek-bytes-avail!* bytes skip evt port)])
        (if (eq? n 0)
            (wrap-evt port (lambda (v) 0))
            n)))
    (lambda ()
      (unless disconnected?
        (when close?
          (close-input-port port))))
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
     [(mode) (check-disconnect) (file-stream-buffer-mode port 
                                                         (if (eq? mode 'line) 'block mode))]))
   (lambda ()
     (set! disconnected? #t)
     port)))

(define (make-disconnectable-output-port port close?)
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
      (if (= start end)
          (begin
            (flush-output port)
            0)
          (cond
           [enable-breaks?
            (parameterize-break #t (write-bytes (subbytes bytes start end) port))]
           [can-buffer/block?
            (write-bytes (subbytes bytes start end) port)]
           [else
            (write-bytes-avail* (subbytes bytes start end) port)])))
    (lambda ()
      (unless disconnected?
        (when close?
          (close-output-port port))))
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

;; For merging two kinds of ports:
(define-struct dual-port (in out)
  #:property prop:input-port 0
  #:property prop:output-port 1)

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
(define-struct textual-output-port (port transcoder)
  #:property prop:output-port 0)
(define-struct (textual-input/output-port textual-input-port) (out-port)
  #:property prop:output-port 0)

(define (port-transcoder port)
  (cond
   [(dual-port? port) (port-transcoder (dual-port-in port))]
   [(textual-input-port? port) (textual-input-port-transcoder port)]
   [(textual-output-port? port) (textual-output-port-transcoder port)]
   [(input-port? port) #f]
   [(output-port? port) #f]
   [else (raise-type-error 'port-transcoder "port" port)]))

(define (textual-port? v)
  (if (port? v)
      (or (textual-input-port? v)
          (textual-output-port? v)
          (and (dual-port? v) 
               (textual-port? (dual-port-in v))))
      (raise-type-error 'textual-port? "port" v)))

(define (binary-port? v)
  (if (port? v)
      (not (or (textual-input-port? v)
               (textual-output-port? v)
               (and (dual-port? v) 
                    (textual-port? (dual-port-in v)))))
      (raise-type-error 'binary-port? "port" v)))

(define (wrap-binary-input-port p get-pos set-pos! close?)
  (let-values ([(p disconnect) (make-disconnectable-input-port p close?)])
    (make-binary-input-port p disconnect get-pos set-pos!)))

(define (wrap-binary-output-port p get-pos set-pos! close?)
  (let-values ([(p disconnect) (make-disconnectable-output-port p close?)])
    (make-binary-output-port p disconnect get-pos set-pos!)))

(define (wrap-binary-input/output-port p get-pos set-pos! close?)
  (let-values ([(p disconnect) (make-disconnectable-input-port p #t)]
               [(out-p out-disconnect) (make-disconnectable-output-port p #t)])
    (make-binary-input/output-port p disconnect get-pos set-pos!
                                   out-p out-disconnect)))

(define (no-op-transcoder? t)
  (or (eq? t utf8-transcoder)
      (and (eq? utf-8 (transcoder-codec t))
           (eq? (transcoder-eol-style t) 'none)
           (eq? 'replace (transcoder-error-handling-mode t)))))

(define (transcode-input p t)
  (let ([p (if (binary-input-port? p)
               ((binary-input-port-disconnect p))
               p)])
    (if (no-op-transcoder? t)
        p
        (letrec ([self
                  (reencode-input-port p 
                                       (codec-enc (transcoder-codec t))
                                       (case (transcoder-error-handling-mode t)
                                         [(raise) #f]
                                         [(ignore) #""]
                                         [(replace) (string->bytes/utf-8 "\uFFFD")])
                                       #t
                                       (object-name p)
                                       (not (eq? (transcoder-eol-style t) 'none))
                                       (lambda (msg port)
                                         (raise
                                          (condition
                                           (make-message-condition 
                                            (format "~a: ~e" msg port))
                                           (make-i/o-decoding-error
                                            self)))))])
          self))))

(define (transcode-output p t)
  (let ([p (cond
            [(binary-output-port? p)
             ((binary-output-port-disconnect p))]
            [(binary-input/output-port? p)
             ((binary-input/output-port-out-disconnect p))]
            [else p])])
    (if (no-op-transcoder? t)
        p
        (letrec ([self
                  (reencode-output-port p 
                                        (codec-enc (transcoder-codec t))
                                        (case (transcoder-error-handling-mode t)
                                          [(raise) #f]
                                          [(ignore) #""]
                                          [(replace) (string->bytes/utf-8 "\uFFFD")])
                                        #t
                                        (object-name p)
                                        (case (transcoder-eol-style t)
                                          [(lf none) #f]
                                          [(cr) #"\r"]
                                          [(crlf) #"\r\n"]
                                          [(nel) (string->bytes/utf-8 "\u85")]
                                          [(crnel) (string->bytes/utf-8 "\r\u85")]
                                          [(ls) (string->bytes/utf-8 "\u2028")]
                                          [else (error 'transcoded-port "unknown eol style: ~e" 
                                                       (transcoder-eol-style t))])
                                        (lambda (msg port)
                                          (raise
                                           (condition
                                            (make-message-condition 
                                             (format "~a: ~e" msg port))
                                            (make-i/o-encoding-error
                                             self
                                             #\?)))))])
          self))))

(define (transcoded-port p t)
  (unless (and (port? p)
               (binary-port? p))
    (raise-type-error 'transcoded-port "binary port" p))
  (unless (transcoder? t)
    (raise-type-error 'transcoded-port "transcoder" t))
  (cond
   [(and (input-port? p) (output-port? p))
    (make-textual-input/output-port (transcode-input p t)
                                    t 
                                    (transcode-output p t))]
   [(input-port? p)
    (make-textual-input-port (transcode-input p t) t)]
   [(output-port? p)
    (make-textual-output-port (transcode-output p t) t)]))

(define (port-has-port-position? p)
  (unless (port? p)
    (raise-type-error 'port-has-port-position? "port" p))
  (cond
   [(binary-input-port? p)
    (and (binary-input-port-get-pos p) #t)]
   [(binary-output-port? p)
    (and (binary-output-port-get-pos p) #t)]
   [(textual-input-port? p)
    (port-has-port-position? (textual-input-port-port p))]
   [(textual-output-port? p)
    (port-has-port-position? (textual-output-port-port p))]
   [(dual-port? p)
    (port-has-port-position? (dual-port-in p))]
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
   [(dual-port? p)
    (port-position (dual-port-in p))]
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
   [(dual-port? p)
    (port-has-set-port-position!? (dual-port-in p))]
   [else
    ;; we could also allow string ports here
    (file-stream-port? p)]))

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
   [(dual-port? p)
    (set-port-position! (dual-port-in p) pos)]
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
    (file-stream-buffer-mode p (if (eq? buffer-mode 'line)
                                   'block
                                   buffer-mode))
    (if maybe-transcoder
        (transcoded-port p maybe-transcoder)
        (wrap-binary-input-port p 
                                (lambda () (file-position p)) 
                                (lambda (pos) (file-position p pos))
                                #t))))

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
                                (lambda (pos) (file-position p pos))
                                #t))))

(define (open-string-input-port str)
  (unless (string? str)
    (raise-type-error 'open-bytevector-input-port "string" str))
  (let ([p (open-input-string str)])
    (transcoded-port
     (wrap-binary-input-port p
                             (lambda () (file-position p)) 
                             (lambda (pos) (file-position p pos))
                             #t)
     utf8-transcoder)))

(define standard-input-port
  (let ([p (current-input-port)])
    (lambda ()
      (wrap-binary-input-port p
                              (lambda () (file-position p)) 
                              (lambda (pos) (file-position p pos))
                              #f))))

(define input-ports (make-weak-hasheq))

(define (r6rs:current-input-port)
  (let ([p (current-input-port)])
    (cond
     [(textual-port? p) p]
     [(hash-ref input-ports p #f)
      => ephemeron-value]
     [else
      (let ([p2 (transcoded-port p utf8-transcoder)])
        (hash-set! input-ports p (make-ephemeron p p2))
        p2)])))

(define (make-custom-binary-input-port id read! get-position set-position! close)
  (let* ([peeked 0]
         [p (make-input-port/read-to-peek
             id
             (lambda (bytes)
               (let ([v (read! bytes 0 (bytes-length bytes))])
                 (set! peeked (+ peeked v))
                 (if (zero? v)
                     eof
                     v)))
             #f
             (or close void)
             #f void 1
             #f #f
             (lambda (consumed-n)
               (unless (eof-object? consumed-n)
                 (set! peeked (- consumed-n 1)))))])
    (wrap-binary-input-port p
                            (and get-position
                                 (lambda ()
                                   (let ([v (get-position)])
                                     (- v peeked))))
                            (and set-position!
                                 (lambda (pos)
                                   ;; flush peeked
                                   (let loop ()
                                     (unless (zero? peeked)
                                       (read-byte-or-special p)
                                       (loop)))
                                   ;; set position
                                   (set-position! pos)))
                            #t)))


(define (make-custom-textual-input-port id read! get-position set-position! close)
  (make-textual-input-port
   (make-custom-binary-input-port
    id 
    (let-values ([(in out) (make-pipe)])
      (lambda (bstr offset len)
        (let loop ()
          (let ([n (read-bytes-avail!* bstr in offset len)])
            (if (zero? n)
                (let ([str (make-string (bytes-length bstr))])
                  (let ([len (read! str 0 (bytes-length bstr))])
                    (if (zero? len)
                        0
                        (begin
                          (write-string (substring str 0 len) out)
                          (loop)))))
                n)))))
    get-position
    set-position!
    (or close void))
   #f))

;; ----------------------------------------

(define (get-u8 p)
  (unless (binary-port? p)
    (raise-type-error 'get-u8 "binary port" p))
  (read-byte p))

(define (lookahead-u8 p)
  (unless (binary-port? p)
    (raise-type-error 'lookahead-u8 "binary port" p))
  (peek-byte p 0))

(define (get-bytevector-n p cnt)
  (unless (binary-port? p)
    (raise-type-error 'get-bytevector-n "binary port" p))
  (read-bytes cnt p))

(define (get-bytevector-n! p bytes start end)
  (unless (binary-port? p)
    (raise-type-error 'get-bytevector-n! "binary port" p))
  (read-bytes! bytes p start end))

(define (get-bytevector-some p)
  (unless (binary-port? p)
    (raise-type-error 'get-bytevector-some "binary port" p))
  (let ([bytes (make-bytes 4096)])
    (let ([n (read-bytes-avail! bytes p)])
      (if (eof-object? n)
          n
          (subbytes bytes 0 n)))))

(define (get-bytevector-all p)
  (unless (binary-port? p)
    (raise-type-error 'get-bytevector-all "binary port" p))
  (let ([p2 (open-output-bytes)])
    (copy-port p p2)
    (let ([s (get-output-bytes p2 #t)])
      (if (zero? (bytes-length s))
          eof
          s))))

;; ----------------------------------------

(define (get-char p)
  (unless (textual-port? p)
    (raise-type-error 'get-char "textual port" p))
  (read-char p))

(define (lookahead-char p)
  (unless (textual-port? p)
    (raise-type-error 'lookahead-char "textual port" p))
  (peek-char p))

(define (get-string-n p cnt)
  (unless (textual-port? p)
    (raise-type-error 'get-string-n "textual port" p))
  (read-string cnt p))

(define (get-string-n! p str start end)
  (unless (textual-port? p)
    (raise-type-error 'get-string-n! "textual port" p))
  (read-string! str p start end))

(define (get-string-all p)
  (unless (textual-port? p)
    (raise-type-error 'get-string-all "textual port" p))
  (let ([p2 (open-output-bytes)])
    (copy-port p p2)
    (get-output-string p2)))

(define (get-line p)
  (unless (textual-port? p)
    (raise-type-error 'get-line "textual port" p))
  (read-line p 'linefeed))

(define (get-datum p)
  (unless (textual-port? p)
    (raise-type-error 'get-datum "textual port" p))
  (let loop ([v (with-r6rs-reader-parameters (lambda () (read p)))])
    (cond
     [(pair? v) (mcons (loop (car v))
                       (loop (cdr v)))]
     [(vector? v) (list->vector
                   (map loop (vector->list v)))]
     [else v])))

;; ----------------------------------------

(define (flush-output-port p)
  (flush-output p))

(define (output-port-buffer-mode p)
  (file-stream-buffer-mode p))

(define (do-open-file-output-port who
                                  filename 
                                  options
                                  buffer-mode
                                  maybe-transcoder
                                  open-output-file
                                  file-position
                                  wrap-binary-port)
  (unless (enum-set=? (enum-set-universe options)
                      (enum-set-universe (file-options)))
    (raise-type-error who "file-options enum set" options))
  (unless (enum-set-member? buffer-mode (-buffer-modes none line block))
    (raise-type-error who "'none, 'line, or 'block" buffer-mode))
  (when maybe-transcoder
    (unless (transcoder? maybe-transcoder)
      (raise-type-error who "transcoder or #f" maybe-transcoder)))
  (let ([exists-mode (cond
                      [(or (enum-set=? options (file-options no-create no-fail no-truncate))
                           (enum-set=? options (file-options no-create no-truncate)))
                       'update]
                      [(enum-set=? options (file-options no-fail no-truncate))
                       'can-update]
                      [(enum-set-member? 'no-create options) ; no-create, no-create + no-fail
                       'must-truncate]
                      [(enum-set-member? 'no-fail options) ; no-fail
                       'truncate]
                      [else ; no-truncate, <empty>
                       'error])])
    (let ([p (with-handlers ([exn:fail:filesystem?
                              (lambda (exn)
                                (if (and (or (eq? exists-mode 'update)
                                             (eq? exists-mode 'must-truncate))
                                         (not (file-exists? filename)))
                                    (raise
                                     (make-exn:fail:filesystem:exists-not
                                      (exn-message exn)
                                      (exn-continuation-marks exn)
                                      filename))
                                    (raise exn)))])
               (open-output-file filename #:exists exists-mode))])
      (file-stream-buffer-mode p buffer-mode)
      (if maybe-transcoder
          (transcoded-port p maybe-transcoder)
          (wrap-binary-port p 
                            (and file-position (lambda () (file-position p)))
                            (and file-position (lambda (pos) (file-position p pos)))
                            #t)))))

(define (open-file-output-port filename 
                               [options (file-options)]
                               [buffer-mode 'block]
                               [maybe-transcoder #f])
  (do-open-file-output-port 'open-file-output-port
                            filename 
                            options
                            buffer-mode
                            maybe-transcoder
                            open-output-file
                            file-position
                            wrap-binary-output-port))

(define (open-bytevector-output-port [maybe-transcoder #f])
  (when maybe-transcoder
    (unless (transcoder? maybe-transcoder)
      (raise-type-error 'open-bytevector-output-port "transcoder or #f" maybe-transcoder)))
  (let* ([p (open-output-bytes)]
         [p2 (if maybe-transcoder
                 (transcoded-port p maybe-transcoder)
                 (wrap-binary-output-port p
                                          (lambda () (file-position p)) 
                                          (lambda (pos) (file-position p pos))
                                          #t))])
    (values
     p2
     (lambda () 
       (unless (port-closed? p2)
         (flush-output p2))
       (get-output-bytes p #t)))))

(define (call-with-bytevector-output-port proc [maybe-transcoder #f])
  (let-values ([(p get) (open-bytevector-output-port maybe-transcoder)])
    (proc p)
    (close-output-port p)
    (get)))

(define (open-string-output-port)
  (let ([p (open-output-string)])
    (values
     (transcoded-port p utf8-transcoder)
     (lambda ()
       (bytes->string/utf-8 (get-output-bytes p #t))))))

(define (call-with-string-output-port proc)
  (let-values ([(p get) (open-string-output-port)])
    (proc p)
    (close-output-port p)
    (get)))

(define standard-output-port
  (let ([p (current-output-port)])
    (lambda ()
      (wrap-binary-output-port p
                               (lambda () (file-position p)) 
                               (lambda (pos) (file-position p pos))
                               #f))))


(define standard-error-port
  (let ([p (current-error-port)])
    (lambda ()
      (wrap-binary-output-port p
                               (lambda () (file-position p)) 
                               (lambda (pos) (file-position p pos))
                               #f))))

(define output-ports (make-weak-hasheq))

(define (r6rs:current-output-port)
  (convert-output-port (current-output-port)))

(define (r6rs:current-error-port)
  (convert-output-port (current-error-port)))

(define (convert-output-port p)
  (cond
   [(textual-port? p) p]
   [(hash-ref output-ports p #f)
    => ephemeron-value]
   [else
    (let ([p2 (transcoded-port p utf8-transcoder)])
      (hash-set! output-ports p (make-ephemeron p p2))
      p2)]))

(define (make-custom-binary-output-port id write! get-position set-position! close)
  (wrap-binary-output-port
   (make-output-port
    id
    always-evt ;; assuming that it never blocks!
    (lambda (bytes start end can-block/buffer? enable-break?)
      (if (= start end)
          0
          (write! bytes start end)))
    (or close void)
    #f
    #f
    #f
    #f
    void
    1
    #f)
   get-position
   set-position!
   #t))
   
(define (make-custom-textual-output-port id write! get-position set-position! close)
  (make-textual-output-port
   (wrap-binary-output-port
    (make-output-port
     id
     always-evt ;; assuming that it never blocks!
     (let-values ([(in out) (make-pipe)]
                  [(c) #f]
                  [(cvt-buffer) #f]
                  [(buffer) #f])
       (lambda (bytes start end can-block/buffer? enable-break?)
         (let ([direct? (zero? (pipe-content-length in))])
           (if (and direct?
                    (bytes-utf-8-length bytes #f start end))
               ;; No old bytes saved, and bytes to write form a complete
               ;; UTF-8 encoding, so we can write directly:
               (let* ([s (bytes->string/utf-8 bytes #f start end)]
                      [len (string-length s)])
                 (when (positive? len)
                   (write! s 0 len)))
               ;; Partial or need to use existing bytes, so use pipe
               (begin
                 (write-bytes bytes out start end)
                 (unless buffer
                   (set! c (bytes-open-converter "UTF-8-permissive" "UTF-8"))
                   (set! buffer (make-bytes 4096))
                   (set! cvt-buffer (make-bytes 4096)))
                 (let loop ()
                   (let ([n (peek-bytes-avail!* buffer 0 in)])
                     (let ([more? ((pipe-content-length in) . > . n)])
                       (let-values ([(amt used status) (bytes-convert c buffer 0 n cvt-buffer)])
                         (when (positive? amt)
                           (read-bytes! buffer in 0 amt)
                           (let* ([s (bytes->string/utf-8 buffer #f 0 amt)]
                                  [len (string-length s)])
                             (when (positive? len)
                               (write! s 0 (string-length s)))))
                         (when (eq? status 'error)
                           ;; Discard an erroneous byte
                           (read-byte in))
                         ;; Loop 
                         (unless (and (eq? status 'complete)
                                      (not more?))
                           (loop)))))))))
         (- end start)))
     (or close void)
     #f
     #f
     #f
     #f
     void
     1
     #f)
    get-position
    set-position!
    #t)
   #f))

;; ----------------------------------------

(define (put-u8 port b)
  (unless (binary-port? port)
    (raise-type-error 'put-u8 "binary port" port))
  (write-byte b port))

(define (put-bytevector port bytes [start 0] [count (- (bytes-length bytes) start)])
  (unless (binary-port? port)
    (raise-type-error 'put-bytevector "binary port" port))
  (write-bytes bytes port start (+ start count)))

(define (put-char port ch)
  (unless (textual-port? port)
    (raise-type-error 'put-u8 "textual port" port))
  (write-char ch port))

(define (put-string port str [start 0] [count (- (string-length str) start)])
  (unless (textual-port? port)
    (raise-type-error 'put-string "textual port" port))
  (write-string (substring str start (+ start count)) port))

(define (put-datum port v)
  (unless (textual-port? port)
    (raise-type-error 'put-datum "textual port" port))
  (parameterize ([print-mpair-curly-braces #f]
                 [pretty-print-columns 'infinity]
                 [pretty-print-size-hook
                  (lambda (v write? p)
                    (cond
                     [(symbol? v)
                      (let ([s (symbol->string v)])
                        (and (not (regexp-match (force rx:id) s))
                             (for/fold ([len 0])
                                 ([c (in-string s)]
                                  [pos (in-naturals)])
                               (+ len 
                                  (if (or (char-alphabetic? c)
                                          (and (char-numeric? c)
                                               (positive? pos)))
                                      1
                                      (+ 3 (string-length 
                                            (number->string (char->integer c) 16))))))))]
                     [(string? v) 
                      (and (for/or ([c (in-string v)])
                                   (not (or (char-graphic? c)
                                            (char-blank? c))))
                           (for/fold ([w 2])
                               ([c (in-string v)])
                             (cond
                              [(eq? c #\") 2]
                              [(eq? c #\\) 2]
                              [(char-graphic? c) 1]
                              [(char-blank? c) 1]
                              [(eq? c #\newline) 2]
                              [(eq? c #\return) 2]
                              [else 9])))]
                     [(char? v)
                      (case v
                        [(#\u7) 7] ; #\alarm
                        [(#\u1B) 5] ; #\esc
                        [(#\u7F) 8] ; #\delete
                        [else (and (not (char-graphic? v))
                                   (+ 3
                                      (if ((char->integer v) . < . #x10000)
                                          4
                                          6)))])]
                     [(bytes? v) (+ 5
                                    (sub1 (bytes-length v))
                                    (for/fold ([len 0])
                                        ([b (in-bytes v)])
                                      (+ len (cond
                                              [(b . < . 10) 1]
                                              [(b . < . 100) 2]
                                              [else 3]))))]
                     [else #f]))]
                 [pretty-print-print-hook
                  (lambda (v write? p)
                    (cond
                     [(symbol? v)
                      (for ([c (in-string (symbol->string v))]
                            [pos (in-naturals)])
                        (if (or (char-alphabetic? c)
                                (and (char-numeric? c)
                                     (positive? pos)))
                            (display c p)
                            (begin
                              (display "\\x" p)
                              (display (number->string (char->integer c) 16) p)
                              (display ";" p))))]
                     [(string? v) 
                      (write-char #\" p)
                      (for ([c (in-string v)])
                        (cond
                         [(eq? c #\") (display "\\\"" p)]
                         [(eq? c #\\) (display "\\\\" p)]
                         [(char-graphic? c) (write-char c p)]
                         [(char-blank? c) (write-char c p)]
                         [(eq? c #\newline) (display "\\n" p)]
                         [(eq? c #\return) (display "\\r" p)]
                         [else
                          (display "\\x" p)
                          (let ([s (format "00000~x" (char->integer c))])
                            (display (substring s (- (string-length s) 6)) p)
                            (write-char #\; p))]))
                      (write-char #\" p)]
                     [(char? v)
                      (case v
                        [(#\u7) (display "#\\alarm" p)]
                        [(#\u1B) (display "#\\esc" p)]
                        [(#\u7F) (display "#\\delete" p)]
                        [else 
                         (display "#\\x" p)
                         (let ([n (number->string (char->integer v) 16)])
                           (display (make-string
                                     (- (if ((string-length n) . <= . 4)
                                            4
                                            6)
                                        (string-length n))
                                     #\0)
                                    p)
                           (display n p))])]
                     [(bytes? v)
                      (display "#vu8(" p)
                      (if (zero? (bytes-length v))
                          (display ")" p)
                          (begin
                            (display (bytes-ref v 0) p)
                            (for ([b (in-bytes v)]
                                  [i (in-naturals)])
                              (unless (zero? i)
                                (display " " p)
                                (display b p)))
                            (display ")" p)))]))])
    (pretty-write v port)))

;; ----------------------------------------

(define (open-file-input/output-port filename 
                                     [options (file-options)]
                                     [buffer-mode 'block]
                                     [maybe-transcoder #f])
  (do-open-file-output-port 'open-file-input/output-port
                            filename 
                            options
                            (if (eq? buffer-mode 'line)
                                'block
                                buffer-mode)
                            maybe-transcoder
                            (lambda (name #:exists mode)
                              (let-values ([(in out) (open-input-output-file name #:exists mode)])
                                (file-stream-buffer-mode out buffer-mode)
                                (make-dual-port in out)))
                            ;; Input and output buffering make `file-position' iffy.
                            (if (eq? buffer-mode 'none)
                                (case-lambda
                                 [(p) (file-position (dual-port-in p))]
                                 [(p pos) 
                                  (flush-output p)
                                  (file-position (dual-port-in p) pos)])
                                #f)
                            wrap-binary-input/output-port))

(define (make-make-custom-input/output-port
         make-custom-input-port
         make-custom-output-port)
  (lambda (id read! write! get-pos set-pos! close)
    (let* ([closed-one? #f]
           [close (and close
                       (lambda ()
                         (if closed-one?
                             (close)
                             (set! closed-one? #t))))])
      (let ([in (make-custom-input-port id read! get-pos set-pos! close)]
            [out (make-custom-output-port id write! get-pos set-pos! close)])
        (make-dual-port in out)))))

(define make-custom-binary-input/output-port
  (make-make-custom-input/output-port
   make-custom-binary-input-port
   make-custom-binary-output-port))

(define make-custom-textual-input/output-port
  (make-make-custom-input/output-port
   make-custom-textual-input-port
   make-custom-textual-output-port))

(define (bytevector->string bv t)
  (unless (transcoder? t)
    (raise-type-error 'bytevector->string "transcoder" t))
  (let ([p #f])
    (dynamic-wind
     (lambda () 
       (set! p (open-bytevector-input-port bv t)))
     (lambda ()
       (apply
        string-append
        (let loop ()
          (let ([s (get-string-n p 4096)])
            (if (eof-object? s)
                null
                (cons s (loop)))))))
     (lambda () (close-input-port p)))))
                   
(define (string->bytevector s t)
  (unless (transcoder? t)
    (raise-type-error 'string->bytevector "transcoder" t))
  (let ([p #f]
        [result #f])
    (dynamic-wind
     (lambda () 
       (set!-values (p result) (open-bytevector-output-port t)))
     (lambda ()
       (put-string p s)
       (result))
     (lambda () (close-output-port p)))))

;; ----------------------------------------

(define (r6rs-port->port p)
  (cond [(binary-input-port? p)
         ((binary-input-port-disconnect p))]
        [(binary-output-port? p)
         ((binary-output-port-disconnect p))]
        [else
         (raise-type-error 'r6rs-port->port "binary input or output port" p)]))
