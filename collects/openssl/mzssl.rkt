
;; Disabled when `enforce-retry?' is #f:
;;  Warn clients: when a (non-blocking) write fails to write all the
;;   data, the stream is actually committed to writing the given data
;;   in the future. (This requirement comes from the SSL library.)

;; Another warning: data that is written and not buffered may still be
;;  in flight between Racket and the underlying ports. A `flush-output'
;;  won't return until sent data is actually in the underlying port.
;;  (This is due to the fact that unbuffered data cannot be written
;;  without blocking.)

;; One last warning: a write/read must block because a previous
;;  read/write (the opposite direction) didn't finish, and so that
;;  opposite must be completed, first.

(module mzssl racket/base
  (require ffi/unsafe
           ffi/unsafe/define
           ffi/unsafe/atomic
           racket/port
           racket/tcp
           "libcrypto.rkt"
           "libssl.rkt")

  (provide ssl-available?
	   ssl-load-fail-reason

	   ssl-make-client-context
	   ssl-make-server-context
	   ssl-client-context?
	   ssl-server-context?
	   ssl-context?

	   ssl-load-certificate-chain!
	   ssl-load-private-key!
	   ssl-load-verify-root-certificates!
	   ssl-load-suggested-certificate-authorities!

     ssl-set-verify!
           
     ;sets the ssl server to try an verify certificates
     ;it does not require verification though.
     ssl-try-verify!

     ;call on an ssl port, this will return true if the peer
     ;presented a valid certificate and was verified
     ssl-peer-verified?
     ssl-peer-subject-name
     ssl-peer-issuer-name

     ports->ssl-ports

	   ssl-listen
	   ssl-close
	   ssl-accept
	   ssl-accept/enable-break
	   ssl-connect
	   ssl-connect/enable-break

	   ssl-listener?
	   ssl-addresses
	   ssl-abandon-port

           ssl-port?)

  (define ssl-load-fail-reason
    (or libssl-load-fail-reason
        libcrypto-load-fail-reason))

  (define 3m? (eq? '3m (system-type 'gc)))

  (define libmz (ffi-lib #f))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SSL bindings and constants

  (define-ffi-definer define-crypto libcrypto
    #:default-make-fail make-not-available)
  (define-ffi-definer define-ssl libssl
    #:default-make-fail make-not-available)
  (define-ffi-definer define-mzscheme libmz)

  (define-syntax typedef
    (syntax-rules ()
      [(_ id t)
       (define-fun-syntax id (syntax-id-rules () [_ t]))]))

  (typedef _BIO_METHOD* _pointer)
  (typedef _BIO* _pointer)
  (typedef _SSL_METHOD* _pointer)
  (typedef _SSL_CTX* _pointer)
  (typedef _SSL* _pointer)
  (typedef _X509_NAME* _pointer)
  (typedef _X509* _pointer)

  (define-ssl SSLv2_client_method (_fun -> _SSL_METHOD*))
  (define-ssl SSLv2_server_method (_fun -> _SSL_METHOD*))
  (define-ssl SSLv3_client_method (_fun -> _SSL_METHOD*))
  (define-ssl SSLv3_server_method (_fun -> _SSL_METHOD*))
  (define-ssl SSLv23_client_method (_fun -> _SSL_METHOD*))
  (define-ssl SSLv23_server_method (_fun -> _SSL_METHOD*))
  (define-ssl TLSv1_client_method (_fun -> _SSL_METHOD*))
  (define-ssl TLSv1_server_method (_fun -> _SSL_METHOD*))

  (define-crypto BIO_s_mem (_fun -> _BIO_METHOD*))
  (define-crypto BIO_new (_fun _BIO_METHOD* -> _BIO*))
  (define-crypto BIO_free (_fun _BIO* -> _void))

  (define-crypto BIO_read (_fun _BIO* _bytes _int -> _int))
  (define-crypto BIO_write (_fun _BIO* _bytes _int -> _int))
  (define-crypto BIO_ctrl (_fun _BIO* _int _long _long -> _long))
  (define (BIO_set_mem_eof_return b v)
    (BIO_ctrl b BIO_C_SET_BUF_MEM_EOF_RETURN v 0))

  (define-ssl SSL_CTX_new (_fun _SSL_METHOD* -> _SSL_CTX*))
  (define-ssl SSL_CTX_free (_fun _SSL_CTX* -> _void))
  (define-ssl SSL_CTX_ctrl (_fun _SSL_CTX* _int _long _pointer -> _long))
  (define (SSL_CTX_set_mode ctx m)
    (SSL_CTX_ctrl ctx SSL_CTRL_MODE m #f))

  (define-ssl SSL_CTX_set_verify (_fun _SSL_CTX* _int _pointer -> _void))
  (define-ssl SSL_CTX_use_certificate_chain_file (_fun _SSL_CTX* _bytes -> _int))
  (define-ssl SSL_CTX_load_verify_locations (_fun _SSL_CTX* _bytes _pointer -> _int))
  (define-ssl SSL_CTX_set_client_CA_list (_fun _SSL_CTX* _X509_NAME* -> _int))
  (define-ssl SSL_CTX_set_session_id_context (_fun _SSL_CTX* _bytes _int -> _int))
  (define-ssl SSL_CTX_use_RSAPrivateKey_file (_fun _SSL_CTX* _bytes _int -> _int))
  (define-ssl SSL_CTX_use_PrivateKey_file (_fun _SSL_CTX* _bytes _int -> _int))
  (define-ssl SSL_load_client_CA_file (_fun _bytes -> _X509_NAME*))

  (define-ssl SSL_new (_fun _SSL_CTX* -> _SSL*))
  (define-ssl SSL_set_bio (_fun _SSL* _BIO* _BIO* -> _void))
  (define-ssl SSL_connect (_fun _SSL* -> _int))
  (define-ssl SSL_accept (_fun _SSL* -> _int))
  (define-ssl SSL_free (_fun _SSL* -> _void))
  (define-ssl SSL_read (_fun _SSL* _bytes _int -> _int))
  (define-ssl SSL_write (_fun _SSL* _bytes _int -> _int))
  (define-ssl SSL_shutdown (_fun _SSL* -> _int))
  (define-ssl SSL_get_verify_result (_fun _SSL* -> _long))
  (define-ssl SSL_get_peer_certificate (_fun _SSL* -> _X509*))
  
  (define-crypto X509_get_subject_name (_fun _X509* -> _X509_NAME*))
  (define-crypto X509_get_issuer_name (_fun _X509* -> _X509_NAME*))
  (define-crypto X509_NAME_oneline (_fun _X509_NAME* _bytes _int -> _bytes))

  (define-ssl SSL_get_error (_fun _SSL* _int -> _int))

  (define-crypto ERR_get_error (_fun -> _long))
  (define-crypto ERR_error_string_n (_fun _long _bytes _long -> _void))

  (define-ssl SSL_library_init (_fun -> _void))
  (define-ssl SSL_load_error_strings (_fun -> _void))
  
  (define X509_V_OK 0)

  (define SSL_ERROR_WANT_READ 2)
  (define SSL_ERROR_WANT_WRITE 3)
  (define SSL_ERROR_SYSCALL 5)
  (define SSL_ERROR_ZERO_RETURN 6)

  (define BIO_C_SET_BUF_MEM_EOF_RETURN 130)

  (define SSL_FILETYPE_PEM 1)
  (define SSL_FILETYPE_ASN1 2)

  (define SSL_VERIFY_NONE #x00)
  (define SSL_VERIFY_PEER #x01)
  (define SSL_VERIFY_FAIL_IF_NO_PEER_CERT #x02)

  (define SSL_MODE_ENABLE_PARTIAL_WRITE #x01)
  (define SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER #x02)
  (define SSL_CTRL_MODE 33)

  (define-mzscheme scheme_start_atomic (_fun -> _void))
  (define-mzscheme scheme_end_atomic (_fun -> _void))
  (define-mzscheme scheme_make_custodian (_fun _pointer -> _scheme))

  ;; Make this bigger than 4096 to accommodate at least
  ;; 4096 of unencrypted data
  (define BUFFER-SIZE 8000)

  ;; The man pages for SSL_read and SSL_write say that they must be
  ;; retried with the same arguments when they return SSL_ERROR_WANT_READ
  ;; or SSL_ERROR_WANT_WRITE.  This may not actually be true, especially
  ;; when SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER is used, and "retry" may or
  ;; may not mean "retry without doing other things first". Set `enforce-retry?'
  ;; to #t to obey the manpage and retry without doing other things, which
  ;; has an implicitation for clients as noted at the top of this file.
  (define enforce-retry? #f)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Error handling

  (define-syntax with-failure
    (syntax-rules ()
      [(_ thunk body ...)
       (with-handlers ([exn? (lambda (exn)
			       (thunk)
			       (raise exn))])
	 body ...)]))

  (define (get-error-message id)
    (let* ([buffer (make-bytes 512)])
      (ERR_error_string_n id buffer (bytes-length buffer))
      (regexp-match #rx#"^[^\0]*" buffer)))

  (define (check-valid v who what)
    (when (ptr-equal? v #f)
      (let ([id (ERR_get_error)])
	(escape-atomic
	 (lambda ()
	   (error who "~a failed ~a" what (get-error-message id)))))))

  (define (error/network who fmt . args)
    (raise (make-exn:fail:network
            (format "~a: ~a" who (apply format fmt args))
            (current-continuation-marks))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Atomic blocks

  ;; Obviously, be careful in an atomic block. In particular,
  ;; DO NOT CONSTRUCT AN ERROR DIRECTLY IN AN ATOMIC BLOCK,
  ;; because the error message almost certainly involves things
  ;; like a ~a or ~e format, which can trigger all sorts of
  ;; printing extensions. Instead, send a thunk that
  ;; constructs and raises the exception to `escape-atomic'.

  (define in-atomic? (make-parameter #f))
  (define-struct (exn:atomic exn) (thunk))

  (define-syntax atomically
    (syntax-rules ()
      [(_ body ...)
       (parameterize-break
	#f
	(with-handlers ([exn:atomic? (lambda (exn)
				       ((exn:atomic-thunk exn)))])
	  (parameterize ([in-atomic? #t])
	    (dynamic-wind
		(lambda () (scheme_start_atomic))
		(lambda () body ...)
		(lambda () (scheme_end_atomic))))))]))

  (define (escape-atomic thunk)
    (if (in-atomic?)
	(raise (make-exn:atomic 
		"error during atomic..."
		(current-continuation-marks)
		thunk))
	(thunk)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Structs

  (define-struct ssl-context (ctx))
  (define-struct (ssl-client-context ssl-context) ())
  (define-struct (ssl-server-context ssl-context) ())

  (define-struct ssl-listener (l mzctx)
    #:property prop:evt (lambda (lst) (wrap-evt (ssl-listener-l lst) 
                                                (lambda (x) lst))))

  ;; internal:
  (define-struct mzssl (ssl i o r-bio w-bio pipe-r pipe-w 
			    buffer lock 
			    w-closed? r-closed?
			    flushing? must-write must-read
			    refcount 
			    close-original? shutdown-on-close?
			    finalizer-cancel
                            error)
    #:mutable)

  (define (make-immobile-bytes n)
    (if 3m?
	;; Allocate the byte string via malloc:
	(atomically
	 (let* ([p (malloc 'raw n)]
		[s (make-sized-byte-string p n)])
	   (register-finalizer s (lambda (v) (free p)))
	   s))
	;; Normal byte string is immobile:
	(make-bytes n)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Contexts, certificates, etc.

  (define default-encrypt 'sslv2-or-v3)

  (define (encrypt->method who also-expect e client?)
    ((case e
       [(sslv2-or-v3) (if client?
			  SSLv23_client_method
			  SSLv23_server_method)]
       [(sslv2) (if client?
		    SSLv2_client_method
		    SSLv2_server_method)]
       [(sslv3) (if client?
		    SSLv3_client_method
		    SSLv3_server_method)]
       [(tls) (if client?
		  TLSv1_client_method
		  TLSv1_server_method)]
       [else (escape-atomic
	      (lambda ()
		(raise-type-error 
		 who
		 (string-append also-expect "'sslv2-or-v3, 'sslv2, 'sslv3, or 'tls")
		 e)))])))

  (define (make-context who protocol-symbol also-expected client?)
    (let ([meth (encrypt->method who also-expected protocol-symbol client?)])
      (atomically ; so we reliably register the finalizer
       (let ([ctx (SSL_CTX_new meth)])
         (check-valid ctx who "context creation")
         (SSL_CTX_set_mode ctx (bitwise-ior SSL_MODE_ENABLE_PARTIAL_WRITE
                                            SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER))
         (register-finalizer ctx (lambda (v) (SSL_CTX_free v)))
         ((if client? make-ssl-client-context make-ssl-server-context) ctx)))))

  (define (ssl-make-client-context [protocol-symbol default-encrypt])
    (make-context 'ssl-make-client-context protocol-symbol "" #t))

  (define (ssl-make-server-context [protocol-symbol default-encrypt])
    (make-context 'ssl-make-server-context protocol-symbol "" #f))

  (define (get-context who context-or-encrypt-method client?)
    (if (ssl-context? context-or-encrypt-method)
	(ssl-context-ctx context-or-encrypt-method)
	(let ([ctx (SSL_CTX_new (encrypt->method who "context" context-or-encrypt-method client?))])
	  (SSL_CTX_set_mode ctx SSL_MODE_ENABLE_PARTIAL_WRITE)
	  ctx)))

  (define (get-context/listener who ssl-context-or-listener)
    (cond
     [(ssl-context? ssl-context-or-listener)
      (ssl-context-ctx ssl-context-or-listener)]
     [(ssl-listener? ssl-context-or-listener)
      (ssl-context-ctx (ssl-listener-mzctx ssl-context-or-listener))]
     [else
      (raise-type-error who
			"SSL context or listener"
			ssl-context-or-listener)]))

  (define (ssl-load-... who load-it ssl-context-or-listener pathname)
    (let ([ctx (get-context/listener 'ssl-load-certificate-chain!
				     ssl-context-or-listener)])
      (unless (path-string? pathname)
	(raise-type-error 'ssl-load-certificate-chain!
			  "path or string"
			  pathname))
      (let ([path (path->bytes
		   (path->complete-path (cleanse-path pathname)
					(current-directory)))])
	(let ([n (load-it ctx path)])
	  (unless (= n 1)
	    (error who "load failed from: ~e ~a"
		   pathname
		   (get-error-message (ERR_get_error))))))))

  (define (ssl-load-certificate-chain! ssl-context-or-listener pathname)
    (ssl-load-... 'ssl-load-certificate-chain! 
		  SSL_CTX_use_certificate_chain_file
		  ssl-context-or-listener pathname))

  (define (ssl-load-verify-root-certificates! ssl-context-or-listener pathname)
    (ssl-load-... 'ssl-load-verify-root-certificates! 
		  (lambda (a b) (SSL_CTX_load_verify_locations a b #f))
		  ssl-context-or-listener pathname))

  (define (ssl-load-suggested-certificate-authorities! ssl-listener pathname)
    (ssl-load-... 'ssl-load-suggested-certificate-authorities! 
		  (lambda (ctx path)
		    (let ([stk (SSL_load_client_CA_file path)])
		      (if (ptr-equal? stk #f)
			  0
			  (begin
			    (SSL_CTX_set_client_CA_list ctx stk)
			    1))))
		  ssl-listener pathname))

  (define (ssl-load-private-key! ssl-context-or-listener pathname
                                 [rsa? #t] [asn1? #f])
    (ssl-load-...
     'ssl-load-private-key!
     (lambda (ctx path)
       ((if rsa? SSL_CTX_use_RSAPrivateKey_file SSL_CTX_use_PrivateKey_file)
        ctx path
        (if asn1? SSL_FILETYPE_ASN1 SSL_FILETYPE_PEM)))
     ssl-context-or-listener pathname))

  (define (ssl-set-verify! ssl-context-or-listener on?)
    (let ([ctx (get-context/listener 'ssl-set-verify!
				     ssl-context-or-listener)])
      (SSL_CTX_set_verify ctx
			  (if on?
			      (bitwise-ior SSL_VERIFY_PEER 
					   SSL_VERIFY_FAIL_IF_NO_PEER_CERT)
			      SSL_VERIFY_NONE)
			  #f)))
   
  (define (ssl-try-verify! ssl-context-or-listener on?)
    (let ([ctx (get-context/listener 'ssl-set-verify!
				     ssl-context-or-listener)])
      
      ;required by openssl. This is more for when calling i2d_SSL_SESSION/d2i_SSL_SESSION
      ;for instance if we were saving sessions in a database etc... We aren't using that
      ;so a generic session name should be fine.
      (let ([bytes #"racket"])
        (SSL_CTX_set_session_id_context ctx bytes (bytes-length bytes)))
      
      (SSL_CTX_set_verify ctx
                          (if on?
                              SSL_VERIFY_PEER
                              SSL_VERIFY_NONE)
                          #f)))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SSL ports

  (define (mzssl-release mzssl)
    ;; Lock must be held
    (set-mzssl-refcount! mzssl (sub1 (mzssl-refcount mzssl)))
    (when (zero? (mzssl-refcount mzssl))
      (atomically
       (set-box! (mzssl-finalizer-cancel mzssl) #f)
       (SSL_free (mzssl-ssl mzssl)))
      (when (mzssl-close-original? mzssl)
	(close-input-port (mzssl-i mzssl))
	(close-output-port (mzssl-o mzssl)))))

  (define (pump-input-once mzssl need-progress?/out)
    (let ([buffer (mzssl-buffer mzssl)]
	  [i (mzssl-i mzssl)]
	  [r-bio (mzssl-r-bio mzssl)])
      (let ([n ((if (and need-progress?/out 
			 (not (output-port? need-progress?/out)))
		    read-bytes-avail! 
		    read-bytes-avail!*)
		buffer i)])
	(cond
	 [(eof-object? n) 
	  (BIO_set_mem_eof_return r-bio 0)
	  eof]
	 [(zero? n)
	  (when need-progress?/out
	    (sync need-progress?/out i))
	  0]
	 [else 
	  (let ([m (BIO_write r-bio buffer n)])
	    (unless (= m n)
	      ((mzssl-error mzssl) 'pump-input-once "couldn't write all bytes to BIO!"))
	    m)]))))

  (define (pump-output-once mzssl need-progress? output-blocked-result)
    (let ([buffer (mzssl-buffer mzssl)]
	  [pipe-r (mzssl-pipe-r mzssl)]
	  [pipe-w (mzssl-pipe-w mzssl)]
	  [o (mzssl-o mzssl)]
	  [w-bio (mzssl-w-bio mzssl)])
      (let ([n (peek-bytes-avail!* buffer 0 #f pipe-r)])
	(if (zero? n)
	    (let ([n (BIO_read w-bio buffer (bytes-length buffer))])
	      (if (n . <= . 0)
		  (begin
		    (when need-progress?
		      ((mzssl-error mzssl) 'pump-output-once "no output to pump!"))
		    #f)
		  (begin
		    (write-bytes buffer pipe-w 0 n)
		    (pump-output-once mzssl need-progress? output-blocked-result))))
	    (let ([n ((if need-progress? write-bytes-avail write-bytes-avail*) buffer o 0 n)])
	      (if (zero? n)
		  output-blocked-result
		  (begin
		    (port-commit-peeked n (port-progress-evt pipe-r) always-evt pipe-r)
		    #t)))))))

  ;; result is #t if there's more data to send out the
  ;;  underlying output port, but the port is full
  (define (pump-output mzssl)
    (let ([v (pump-output-once mzssl #f 'blocked)])
      (if (eq? v 'blocked)
	  #t
	  (and v
	       (pump-output mzssl)))))

  (define (make-ssl-input-port mzssl)
    ;; If SSL_read produces NEED_READ or NEED_WRITE, then the next
    ;;  call to SSL_read must use the same arguments.
    ;; Use xfer-buffer so we have a consistent buffer to use with
    ;;  SSL_read across calls to the port's write function.
    (let-values ([(xfer-buffer) (make-immobile-bytes BUFFER-SIZE)]
		 [(got-r got-w) (make-pipe)]
		 [(must-read-len) #f])
      (make-input-port/read-to-peek
       (format "SSL ~a" (object-name (mzssl-i mzssl)))
       ;; read proc:
       (letrec ([do-read
		 (lambda (buffer)
		   (let ([out-blocked? (pump-output mzssl)]
			 [len (or must-read-len (min (bytes-length xfer-buffer)
						     (bytes-length buffer)))])
		     (let ([n (SSL_read (mzssl-ssl mzssl) xfer-buffer len)])
		       (if (n . >= . 1)
			   (begin
			     (set! must-read-len #f)
			     (if must-read-len
				 ;; If we were forced to try to read a certain amount,
				 ;; then we may have read too much for the immediate
				 ;; request.
				 (let ([orig-n (bytes-length buffer)])
				   (bytes-copy! buffer 0 xfer-buffer 0 (min n orig-n))
				   (when (n . > . orig-n)
				     (write-bytes buffer got-w orig-n n)))
				 (bytes-copy! buffer 0 xfer-buffer 0 n))
			     n)
			   (let ([err (SSL_get_error (mzssl-ssl mzssl) n)])
			     (cond
			      [(or (= err SSL_ERROR_ZERO_RETURN)
				   (and (= err SSL_ERROR_SYSCALL) (zero? n)))
			       ;; We hit the end-of-file
			       (set! must-read-len #f)
			       eof]
			      [(= err SSL_ERROR_WANT_READ)
			       (when enforce-retry?
                                 (set! must-read-len len))
			       (let ([n (pump-input-once mzssl #f)])
				 (if (eq? n 0)
				     (begin
                                       (when enforce-retry?
                                         (set-mzssl-must-read! mzssl (make-semaphore)))
				       (wrap-evt (choice-evt
						  (mzssl-i mzssl) 
						  (if out-blocked?
						      (mzssl-o mzssl) 
						      never-evt))
						 (lambda (x) 0)))
				     (do-read buffer)))]
			      [(= err SSL_ERROR_WANT_WRITE)
                               (when enforce-retry?
                                 (set! must-read-len len))
			       (if (pump-output-once mzssl #f #f)
				   (do-read buffer)
				   (begin
                                     (when enforce-retry?
                                       (set-mzssl-must-read! mzssl (make-semaphore)))
				     (wrap-evt (mzssl-o mzssl) (lambda (x) 0))))]
			      [else
                               (set! must-read-len #f)
			       ((mzssl-error mzssl) 'read-bytes 
                                "SSL read failed ~a"
                                (get-error-message (ERR_get_error)))]))))))]
		[top-read
		 (lambda (buffer)
		   (cond
		    [(mzssl-flushing? mzssl)
		     ;; Flush in progress; try again later:
		     0]
		    [(mzssl-must-write mzssl)
		     => (lambda (sema)
			  (wrap-evt (semaphore-peek-evt sema) (lambda (x) 0)))]
		    [(mzssl-r-closed? mzssl)
		     0]
		    [else
		     (let ([sema (mzssl-must-read mzssl)])
		       (when sema
			 (set-mzssl-must-read! mzssl #f)
			 (semaphore-post sema)))
		     ;; First, try pipe for previously read data:
		     (let ([n (read-bytes-avail!* buffer got-r)])
		       (if (zero? n)
			   ;; Nothing already read, so use SSL_read:
			   (do-read buffer)
			   ;; Got previously read data:
			   n))]))]
		[lock-unavailable
		 (lambda () (wrap-evt (semaphore-peek-evt (mzssl-lock mzssl))
				      (lambda (x) 0)))])
	 (lambda (buffer)
	   (call-with-semaphore
	    (mzssl-lock mzssl)
	    top-read
	    lock-unavailable
	    buffer)))
       ;; fast peek:
       #f
       ;; close proc:
       (lambda ()
	 (call-with-semaphore
	  (mzssl-lock mzssl)
	  (lambda ()
	    (unless (mzssl-r-closed? mzssl)
	      (set-mzssl-r-closed?! mzssl #t)
	      (mzssl-release mzssl))))))))

  (define (flush-ssl mzssl enable-break?)
    ;; Make sure that this SSL connection has said everything that it
    ;; wants to say --- that is, move data from the SLL output to the
    ;; underlying output port. Depending on the transport, the other end
    ;; may be stuck trying to tell us something before it will listen, 
    ;; so we also have to read in any available information.
    (let loop ()
      (let ([v (pump-input-once mzssl #f)])
	(if (and (number? v) (positive? v))
	    ;; Received some input, so start over
	    (loop)
	    ;; Try sending output
	    (let ([v (pump-output-once mzssl #f 'blocked)])
	      ;; If we sent something, continue tring in case there's more.
	      ;; Further, if we blocked on the underlying output, then
	      ;; wait until either input or output is ready:
	      (when v
		(when (eq? v 'blocked)
		  ((if enable-break? sync/enable-break sync) (mzssl-o mzssl) (mzssl-i mzssl)))
		(loop)))))))

  (define (kernel-thread thunk)
    ;; Since we provide #f to scheme_make_custodian,
    ;;  the custodian is managed directly by the root:
    (parameterize ([current-custodian (scheme_make_custodian #f)])
      (thread thunk)))

  (define (make-ssl-output-port mzssl)
    ;; If SSL_write produces NEED_READ or NEED_WRITE, then the next
    ;;  call to SSL_write must use the same arguments.
    ;; Use xfer-buffer so we have a consistent buffer to use with
    ;;  SSL_write across calls to the port's write function.
    (let ([xfer-buffer (make-immobile-bytes BUFFER-SIZE)]
	  [buffer-mode (or (file-stream-buffer-mode (mzssl-o mzssl)) 'bloack)]
	  [flush-ch (make-channel)]
	  [must-write-len #f])
      ;; This thread mkoves data from the SLL stream to the underlying
      ;; output port, because this port's write prodcue claims that the
      ;; data is flushed if it gets into the SSL stream. In other words,
      ;; this flushing thread is analogous to the OS's job of pushing
      ;; data from a socket through the actual network device. It therefore
      ;; runs with the highest possible custodian:
      (kernel-thread (lambda ()
		       (let loop ()
			 (sync flush-ch)
			 (semaphore-wait (mzssl-lock mzssl))
			 (flush-ssl mzssl #f)
			 (semaphore-post (mzssl-flushing? mzssl))
			 (set-mzssl-flushing?! mzssl #f)
			 (semaphore-post (mzssl-lock mzssl))
			 (loop))))
      ;; Create the output port:
      (make-output-port
       (format "SSL ~a" (object-name (mzssl-o mzssl)))
       (mzssl-o mzssl)
       ;; write proc:
       (letrec ([do-write
		 (lambda (len non-block? enable-break?)
		   (let ([out-blocked? (pump-output mzssl)])
		     (if (zero? len)
			 ;; Flush request; all data is in the SSL
			 ;;  stream, but make sure it's gone
			 ;;  through the ports:
			 (begin
			   (flush-ssl mzssl enable-break?)
			   0)
			 ;; Write request; even if blocking is ok, we treat
			 ;;  it as non-blocking and let Racket handle blocking
			 (let ([n (SSL_write (mzssl-ssl mzssl) xfer-buffer len)])
			   (if (n . > . 0)
			       (begin
				 (set! must-write-len #f)
				 ;; Start flush as necessary:
				 (cond
				  [non-block?
				   ;; We can't block, so start the background thread
				   ;;  to flush from SSL to the underlying ports:
				   (set-mzssl-flushing?! mzssl (make-semaphore))
				   (channel-put flush-ch #t)]
				  [else
				   ;; We're allowed to block, and things seem to
				   ;;  work better if we, try to flush all the way
				   ;;  through (even though we're allowed to buffer):
				   (flush-ssl mzssl enable-break?)])
				 n)
			       (let ([err (SSL_get_error (mzssl-ssl mzssl) n)])
				 (cond
				  [(= err SSL_ERROR_WANT_READ)
                                   (when enforce-retry?
                                     (set! must-write-len len))
				   (let ([n (pump-input-once mzssl #f)])
				     (if (eq? n 0)
					 (begin
                                           (when enforce-retry?
                                             (set-mzssl-must-write! mzssl (make-semaphore)))
					   (wrap-evt (choice-evt
						      (mzssl-i mzssl)
						      (if out-blocked?
							  (mzssl-o mzssl)
							  never-evt))
						     (lambda (x) #f)))
					 (do-write len non-block? enable-break?)))]
				  [(= err SSL_ERROR_WANT_WRITE)
                                   (when enforce-retry?
                                     (set! must-write-len len))
				   (if (pump-output-once mzssl #f #f)
				       (do-write len non-block? enable-break?)
				       (begin
                                         (when enforce-retry?
                                           (set-mzssl-must-write! mzssl (make-semaphore)))
					 (wrap-evt (mzssl-o mzssl) (lambda (x) #f))))]
				  [else
				   (set! must-write-len #f)
				   ((mzssl-error mzssl) 'write-bytes 
                                    "SSL write failed ~a"
                                    (get-error-message (ERR_get_error)))])))))))]
		[top-write
		 (lambda (buffer s e non-block? enable-break?)
		   (cond
		    [(mzssl-flushing? mzssl)
		     ;; Need to wait until flush done
		     (if (= s e)
			 ;; Let the background flush finish:
			 (list (semaphore-peek-evt (mzssl-flushing? mzssl)))
			 ;; Try again later:
			 (wrap-evt always-evt (lambda (v) #f)))]
		    [(mzssl-w-closed? mzssl)
		     #f]
		    [(mzssl-must-read mzssl)
		     ;; Read pending, so wait until it's done:
		     => (lambda (sema)
			  (wrap-evt (semaphore-peek-evt sema) (lambda (x) #f)))]
		    [else
		     ;; Normal write (since no flush is active or read pending):
		     (let ([sema (mzssl-must-write mzssl)])
		       (when sema
			 (set-mzssl-must-write! mzssl #f)
			 (semaphore-post sema)))
		     (let ([len (min (- e s) (bytes-length xfer-buffer))])
		       (if must-write-len
			   ;; Previous SSL_write result obligates certain output:
			   (begin
			     (unless (and (len . >= . must-write-len)
					  (bytes=? (subbytes xfer-buffer 0 must-write-len)
						   (subbytes buffer s (+ s must-write-len))))
			       ((mzssl-error mzssl) 'write-bytes 
                                "SSL output request: ~e different from previous unsatisfied request: ~e"
                                (subbytes buffer s e)
                                (subbytes xfer-buffer 0 must-write-len)))
			     (do-write must-write-len non-block? enable-break?))
			   ;; No previous write obligation:
			   (begin
			     (bytes-copy! xfer-buffer 0 buffer s (+ s len))
			     (do-write len non-block? enable-break?))))]))]
		[lock-unavailable
		 (lambda () (wrap-evt (semaphore-peek-evt (mzssl-lock mzssl))
				      (lambda (x) #f)))])
	 (lambda (buffer s e non-block? enable-break?)
	   (let ([v (call-with-semaphore
		     (mzssl-lock mzssl)
		     top-write
		     lock-unavailable
		     buffer s e non-block? enable-break?)])
	     (if (pair? v)
		 (begin
		   ;; Wait on background flush to implement requested flush
		   (sync (car v))
		   0)
		 v))))
       ;; close proc:
       (letrec ([do-close
		 (lambda ()
		   (cond
		    [(mzssl-flushing? mzssl)
		     (semaphore-peek-evt (mzssl-flushing? mzssl))]
		    [(mzssl-w-closed? mzssl)
		     #f]
		    [else
		     ;; issue shutdown (i.e., EOF on read end)
		     (when (mzssl-shutdown-on-close? mzssl)
		       (let loop ([cnt 0])
			 (let ([out-blocked? (flush-ssl mzssl #f)])
			   (let ([n (SSL_shutdown (mzssl-ssl mzssl))])
			     (unless (= n 1)
			       (let ([err (SSL_get_error (mzssl-ssl mzssl) n)])
				 (cond
				  [(= err SSL_ERROR_WANT_READ)
				   (pump-input-once mzssl (if out-blocked? (mzssl-o mzssl) #t))
				   (loop cnt)]
				  [(= err SSL_ERROR_WANT_WRITE)
				   (pump-output-once mzssl #t #f)
				   (loop cnt)]
				  [else
				   (if (= n 0)
				       ;; When 0 is returned, the SSL object doesn't correctly
				       ;; report what it wants (e.g., a write). Send everything
				       ;; out that we have and try again, up to 10 times.
				       (unless (cnt . >= . 10)
					 (loop (add1 cnt)))
				       ((mzssl-error mzssl) 'read-bytes 
                                        "SSL shutdown failed ~a"
                                        (get-error-message (ERR_get_error))))])))))))
		     (set-mzssl-w-closed?! mzssl #t)
		     (mzssl-release mzssl)
		     #f]))]
		[close-loop
		 (lambda ()
		   (let ([v (call-with-semaphore
			     (mzssl-lock mzssl)
			     do-close)])
		     (if v
			 (begin
			   ;; Wait for background flush to finish:
			   (sync v)
			   (close-loop))
			 v)))])
	 (lambda ()
	   (close-loop)))
       ;; Unimplemented port methods:
       #f #f #f #f
       void 1
       ;; Buffer mode proc:
       (case-lambda
	[() buffer-mode]
	[(mode) (set! buffer-mode mode)]))))

  (define (ports->ssl-ports i o 
                            #:context [context #f]
                            #:encrypt [encrypt default-encrypt]
                            #:mode [mode 'connect]
                            #:close-original? [close-original? #f]
                            #:shutdown-on-close? [shutdown-on-close? #f]
                            #:error/ssl [error/ssl error])
    (wrap-ports 'port->ssl-ports i o (or context encrypt) mode close-original? shutdown-on-close? error/ssl))

  (define (create-ssl who context-or-encrypt-method connect/accept error/ssl)
    (atomically ; so we register the finalizer (and it's ok since everything is non-blocking)
     (let ([ctx (get-context who context-or-encrypt-method (eq? connect/accept 'connect))])
       (check-valid ctx who "context creation")
       (with-failure
	(lambda () (when (and ctx
			      (symbol? context-or-encrypt-method))
		     (SSL_CTX_free ctx)))
	(let ([connect? (case connect/accept
			  [(connect) #t]
			  [(accept) #f]
			  [else
			   (escape-atomic
			    (lambda ()
			      (raise-type-error who "'connect or 'accept" 
						connect/accept)))])]
	      [r-bio (BIO_new (BIO_s_mem))]
	      [w-bio (BIO_new (BIO_s_mem))]
	      [free-bio? #t])
	  (with-failure
	   (lambda () (when free-bio?
			(BIO_free r-bio)
			(BIO_free w-bio)))
	   (unless (or (symbol? context-or-encrypt-method)
		       (if connect?
			   (ssl-client-context? context-or-encrypt-method)
			   (ssl-server-context? context-or-encrypt-method)))
	     (escape-atomic
	      (lambda ()
		(error who
		       "'~a mode requires a ~a context, given: ~e"
		       (if connect? 'connect 'accept)
		       (if connect? "client" "server")
		       context-or-encrypt-method))))
	   (let ([ssl (SSL_new ctx)]
		 [cancel (box #t)])
	     (check-valid ssl who "ssl setup")
	     ;; ssl has a ref count on ctx, so release:
	     (when (symbol? context-or-encrypt-method)
	       (SSL_CTX_free ctx)
	       (set! ctx #f))
	     (with-failure
	      (lambda () (SSL_free ssl))
	      (SSL_set_bio ssl r-bio w-bio)
	      ;; ssl has r-bio & w-bio (no ref count?), so drop it:
	      (set! free-bio? #f)
	      
	      ;; Register a finalizer for ssl:
	      (register-finalizer ssl 
				  (lambda (v)
				    (when (unbox cancel)
				      (SSL_free ssl))))
	      ;; Return SSL and the cancel boxL:
	      (values ssl cancel r-bio w-bio connect?)))))))))

  (define (wrap-ports who i o context-or-encrypt-method connect/accept close? shutdown-on-close? error/ssl)
    (unless (input-port? i)
      (raise-type-error who "input port" i))
    (unless (output-port? o)
      (raise-type-error who "output port" o))
    ;; Create the SSL connection:
    (let-values ([(ssl cancel r-bio w-bio connect?)
		  (create-ssl who context-or-encrypt-method connect/accept error/ssl)])
      ;; connect/accept:
      (let-values ([(buffer) (make-bytes BUFFER-SIZE)]
		   [(pipe-r pipe-w) (make-pipe)]
		   [(cancel) (box #t)])
	(let ([mzssl (make-mzssl ssl i o r-bio w-bio pipe-r pipe-w 
				 buffer (make-semaphore 1) 
				 #f #f
				 #f #f #f 2 
				 close? shutdown-on-close?
				 cancel
                                 error/ssl)])
	  (let loop ()
	    (let ([status (if connect?
			      (SSL_connect ssl)
			      (SSL_accept ssl))])
	      (let ([out-blocked? (pump-output mzssl)])
		(when (status . < . 1)
		  (let ([err (SSL_get_error ssl status)])
		    (cond
		     [(= err SSL_ERROR_WANT_READ)
		      (let ([n (pump-input-once mzssl (if out-blocked? o #t))])
			(when (eof-object? n)
			  (error/ssl who "~a failed (input terminated prematurely)"
                                     (if connect? "connect" "accept"))))
		      (loop)]
		     [(= err SSL_ERROR_WANT_WRITE)
		      (pump-output-once mzssl #t #f)
		      (loop)]
		     [else
		      (error/ssl who "~a failed ~a"
                                 (if connect? "connect" "accept")
                                 (get-error-message (ERR_get_error)))]))))))
	  ;; Connection complete; make ports
	  (values (register (make-ssl-input-port mzssl) mzssl #t)
		  (register (make-ssl-output-port mzssl) mzssl #f))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SSL port registry

  (define ssl-ports (make-weak-hasheq))

  (define (register port mzssl input?)
    (hash-set! ssl-ports port (make-ephemeron port (cons mzssl input?)))
    port)

  (define (lookup who what port)
    (let ([v (hash-ref ssl-ports port #f)])
      (unless v
	(raise-type-error who what port))
      (let ([p (ephemeron-value v)])
	(values (car p) (cdr p)))))

  (define (ssl-addresses p [port-numbers? #f])
    (let-values ([(mzssl input?) (lookup 'ssl-addresses "SSL port or listener" p)])
      (tcp-addresses (if (eq? 'listener input?)
                         (ssl-listener-l mzssl)
                         (if input? (mzssl-i mzssl) (mzssl-o mzssl)))
                     port-numbers?)))

  (define (ssl-abandon-port p)
    (let-values ([(mzssl input?) (lookup 'ssl-abandon-port "SSL output port" p)])
      (when input?
	(raise-type-error 'ssl-abandon-port "SSL output port" p))
      (set-mzssl-shutdown-on-close?! mzssl #f)))
  
  (define (ssl-peer-verified? p)
    (let-values ([(mzssl input?) (lookup 'ssl-peer-verified? "SSL port" p)])
      (and (eq? X509_V_OK (SSL_get_verify_result (mzssl-ssl mzssl)))
           (SSL_get_peer_certificate (mzssl-ssl mzssl))
           #t)))
  
  (define (ssl-peer-subject-name p)
    (let-values ([(mzssl input?) (lookup 'ssl-peer-subject-name "SSL port" p)])
      (let ([cert (SSL_get_peer_certificate (mzssl-ssl mzssl))])
        (if cert
            (let ([bytes (make-bytes 1024 0)])
              (X509_NAME_oneline (X509_get_subject_name cert) bytes (bytes-length bytes)))
            #f))))
  
  (define (ssl-peer-issuer-name p)
    (let-values ([(mzssl input?) (lookup 'ssl-peer-subject-name "SSL port" p)])
      (let ([cert (SSL_get_peer_certificate (mzssl-ssl mzssl))])
        (if cert
            (let ([bytes (make-bytes 1024 0)])
              (X509_NAME_oneline (X509_get_issuer_name cert) bytes (bytes-length bytes)))
            #f))))

  (define (ssl-port? v)
    (and (hash-ref ssl-ports v #f) #t))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SSL listen

  (define (ssl-listen port-k
                      [queue-k 5] [reuse? #f] [hostname-or-#f #f]
                      [protocol-symbol-or-context default-encrypt])
    (let* ([ctx (if (ssl-server-context? protocol-symbol-or-context)
                 protocol-symbol-or-context
                 (make-context 'ssl-listen protocol-symbol-or-context
                               "server context, " #f))]
          [l (tcp-listen port-k queue-k reuse? hostname-or-#f)]
          [ssl-l (make-ssl-listener l ctx)])
      (register ssl-l ssl-l 'listener)))

  (define (ssl-close l)
    (unless (ssl-listener? l)
      (raise-type-error 'ssl-close "SSL listener" l))
    (tcp-close (ssl-listener-l l)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SSL accept

  (define (do-ssl-accept who tcp-accept ssl-listener)
    (let-values ([(i o) (tcp-accept (ssl-listener-l ssl-listener))])
      ;; Obviously, there's a race condition between accepting the
      ;; connections and installing the exception handler below. However,
      ;; if breaks are enabled, then i and o could get lost between
      ;; the time that tcp-accept returns and `i' and `o' are bound,
      ;; anyway. So we can assume that breaks are enabled without loss
      ;; of (additional) resources.
      (with-handlers ([void (lambda (exn)
			      (close-input-port i)
			      (close-output-port o)
			      (raise exn))])
	(wrap-ports who i o (ssl-listener-mzctx ssl-listener) 'accept #t #f error/network))))

  (define (ssl-accept ssl-listener)
    (do-ssl-accept 'ssl-accept tcp-accept ssl-listener))

  (define (ssl-accept/enable-break ssl-listener)
    (do-ssl-accept 'ssl-accept/enable-break tcp-accept/enable-break ssl-listener))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SSL connect

  (define (do-ssl-connect who tcp-connect hostname port-k client-context-or-protocol-symbol)
    (let-values ([(i o) (tcp-connect hostname port-k)])
      ;; See do-ssl-accept for note on race condition here:
      (with-handlers ([void (lambda (exn)
			      (close-input-port i)
			      (close-output-port o)
			      (raise exn))])
	(wrap-ports who i o client-context-or-protocol-symbol 'connect #t #f error/network))))

  (define (ssl-connect
              hostname port-k
              [client-context-or-protocol-symbol default-encrypt])
    (do-ssl-connect 'ssl-connect
                    tcp-connect
                    hostname
                    port-k
                    client-context-or-protocol-symbol))

  (define (ssl-connect/enable-break
           hostname port-k
           [client-context-or-protocol-symbol default-encrypt])
    (do-ssl-connect 'ssl-connect/enable-break
                    tcp-connect/enable-break
                    hostname
                    port-k
                    client-context-or-protocol-symbol))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Initialization

  (define ssl-available? (and libssl #t))


  (define scheme_register_process_global
    (and ssl-available?
         (get-ffi-obj 'scheme_register_process_global #f (_fun _string _pointer -> _pointer))))

  (when ssl-available?
    ;; Make sure only one place tries to initialize OpenSSL,
    ;; and wait in case some other place is currently initializing
    ;; it.
    (begin
      (start-atomic)
      (let* ([done (cast 1 _scheme _pointer)]
             [v (scheme_register_process_global "OpenSSL-support-initializing" done)])
        (if v
            ;; Some other place is initializing:
            (begin
              (end-atomic)
              (let loop ()
                (unless (scheme_register_process_global "OpenSSL-support-initialized" #f)
                  (sleep 0.01) ;; busy wait! --- this should be rare
                  (loop))))
            ;; This place must initialize:
            (begin
              (SSL_library_init)
              (SSL_load_error_strings)
              (scheme_register_process_global "OpenSSL-support-initialized" done)
              (end-atomic)))))))
