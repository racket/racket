#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         racket/draw/unsafe/bstr
         net/base64
	 "../common/queue.rkt"
         "types.rkt"
         "utils.rkt")

(provide 
 (protect-out do-single-instance))

(define unique-lib
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (ffi-lib "libunique-1.0" '("0"))))

(define-ffi-definer define-unique unique-lib
  #:default-make-fail make-not-available)

(define _gsize _ulong)

(define UNIQUE_RESPONSE_OK 1)

(define _UniqueApp _GtkWidget) ; not a widget, but we want to connect a signal
(define _UniqueMessageData (_cpointer 'UniqueMessageData))

(define-unique unique_app_new (_fun _string _string -> _UniqueApp)
  #:fail (lambda () (lambda args #f)))
(define-unique unique_app_add_command (_fun _UniqueApp _string _int -> _void))
(define-unique unique_app_is_running (_fun _UniqueApp -> _gboolean))
(define-unique unique_app_send_message (_fun _UniqueApp _int _UniqueMessageData -> _int))

(define-unique unique_message_data_new (_fun -> _UniqueMessageData))
(define-unique unique_message_data_free (_fun _UniqueMessageData -> _void))
(define-unique unique_message_data_set (_fun _UniqueMessageData _pointer _gsize -> _void))
(define-unique unique_message_data_get (_fun _UniqueMessageData (len : (_ptr o _gsize))
                                             -> (data : _bytes)
                                             -> (scheme_make_sized_byte_string
                                                 data
                                                 len
                                                 0)))

(define-signal-handler connect-message-received "message-received"
  (_fun _UniqueApp _int _UniqueMessageData _uint -> _int)
  (lambda (app cmd data time)
    (let ([d (unique_message_data_get data)])
      (with-handlers ([exn:fail? (lambda (exn)
				   (log-error 
				    (format "error handling single-instance message: ~s"
					    (exn-message exn))))])
	(let* ([p (open-input-bytes d)]
	       [vec (read p)])
	  (for-each
	   queue-file-event
	   (map string->path (vector->list vec))))))
    UNIQUE_RESPONSE_OK))

(define-mz gethostname (_fun _pointer _long -> _int)
  #:fail (lambda () #f))

(define HOSTLEN 256)

(define (build-app-name)
  (let-values ([(path) (simplify-path
                        (path->complete-path
                         (or (find-executable-path (find-system-path 'run-file) #f)
                             (find-system-path 'run-file))
                         (current-directory)))]
               [(host) (or (and gethostname
                                (let ([b (make-bytes HOSTLEN)])
                                  (and (zero? (gethostname b HOSTLEN))
                                       (bytes->string/utf-8 (car (regexp-match #rx#"^[^\0]*" b)) #\?))))
                           "")])
    (string->bytes/utf-8
     (format "org.racket-lang.~a"
             (encode
              (format "~a~a~a" host path (version)))))))

(define (encode s)
  (regexp-replace* #rx"=|\r\n" (base64-encode (string->bytes/utf-8 s)) ""))

(define (send-command-line app)
  (let ([msg (unique_message_data_new)]
        [b (let ([o (open-output-bytes)])
             (write (current-command-line-arguments) o)
             (get-output-bytes o))])
    (unique_message_data_set msg b (bytes-length b))
    (unique_app_send_message app 42 msg)))

(define (do-single-instance)
  (let ([app (unique_app_new (build-app-name) #f)])
    (when app
      (unique_app_add_command app "startup" 42)
      (when (unique_app_is_running app)
        (when (= (send-command-line app)
                 UNIQUE_RESPONSE_OK)
          (exit 0)))
      (void (connect-message-received app)))))
