
;; This is a re-implementation of "mzssl.c" using `(lib "foreign.ss")'.
;; It will soon replace "mzssl.c".

(module mzssl2 mzscheme
  (require (lib "foreign.ss")
	   (lib "port.ss")
	   (lib "etc.ss"))

  (provide ssl-make-client-context
	   ports->ssl-ports)

  (unsafe!)

  (define libssl (ffi-lib "libssl"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SSL bindings and constants

  (define-syntax define-define-X
    (syntax-rules ()
      [(_ id lib)
       (define-syntax (id stx)
	 (syntax-case stx ()
	   [(_ id type)
	    (with-syntax ([str (symbol->string (syntax-e #'id))])
	      #'(define id
		  (get-ffi-obj str lib (_fun . type))))]))]))

  (define-define-X define-ssl libssl)
  (define-define-X define-mzscheme #f)
  
  (define-fun-syntax _BIO_METHOD* (syntax-id-rules () [_ _pointer]))
  (define-fun-syntax _BIO* (syntax-id-rules () [_ _pointer]))
  (define-fun-syntax _SSL_METHOD* (syntax-id-rules () [_ _pointer]))
  (define-fun-syntax _SSL_CTX* (syntax-id-rules () [_ _pointer]))
  (define-fun-syntax _SSL* (syntax-id-rules () [_ _pointer]))

  (define-ssl SSLv2_client_method (-> _SSL_METHOD*))
  (define-ssl SSLv2_server_method (-> _SSL_METHOD*))
  (define-ssl SSLv3_client_method (-> _SSL_METHOD*))
  (define-ssl SSLv3_server_method (-> _SSL_METHOD*))
  (define-ssl SSLv23_client_method (-> _SSL_METHOD*))
  (define-ssl SSLv23_server_method (-> _SSL_METHOD*))
  (define-ssl TLSv1_client_method (-> _SSL_METHOD*))
  (define-ssl TLSv1_server_method (-> _SSL_METHOD*))

  (define-ssl BIO_s_mem (-> _BIO_METHOD*))
  (define-ssl BIO_new (_BIO_METHOD* -> _BIO*))
  (define-ssl BIO_free (_BIO* -> _void))

  (define-ssl BIO_read (_BIO* _bytes _int -> _int))
  (define-ssl BIO_write (_BIO* _bytes _int -> _int))
  (define-ssl BIO_ctrl (_BIO* _int _long _long -> _long))
  (define (BIO_set_mem_eof_return b v)
    (BIO_ctrl b BIO_C_SET_BUF_MEM_EOF_RETURN v 0))

  (define-ssl SSL_CTX_new (_SSL_METHOD* -> _SSL_CTX*))
  (define-ssl SSL_CTX_free (_SSL_CTX* -> _void))

  (define-ssl SSL_new (_SSL_CTX* -> _SSL*))
  (define-ssl SSL_set_bio (_SSL* _BIO* _BIO* -> _void))
  (define-ssl SSL_connect (_SSL* -> _int))
  (define-ssl SSL_accept (_SSL* -> _int))
  (define-ssl SSL_free (_SSL* -> _void))
  (define-ssl SSL_read (_SSL* _bytes _int -> _int))
  (define-ssl SSL_write (_SSL* _bytes _int -> _int))
  (define-ssl SSL_shutdown (_SSL* -> _int))

  (define-ssl SSL_get_error (_SSL* _int -> _int))

  (define-ssl ERR_get_error (-> _long))
  (define-ssl ERR_error_string_n (_long _bytes _long -> _void))

  (define-ssl SSL_library_init (-> _void))
  (define-ssl SSL_load_error_strings (-> _void))

  (define SSL_ERROR_WANT_READ 2)
  (define SSL_ERROR_WANT_WRITE 3)
  (define SSL_ERROR_SYSCALL 5)
  (define SSL_ERROR_ZERO_RETURN 6)

  (define BIO_C_SET_BUF_MEM_EOF_RETURN 130)

  (define-mzscheme scheme_start_atomic (-> _void))
  (define-mzscheme scheme_end_atomic (-> _void))

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
	(error who "~a failed ~a" 
	       what
	       (get-error-message id)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Atomic blocks

  (define-syntax atomically
    (syntax-rules ()
      [(_ body ...)
       (dynamic-wind
	   (lambda () (scheme_start_atomic))
	   (lambda () body ...)
	   (lambda () (scheme_end_atomic)))]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-struct ssl-client-context (ctx))

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
       [else (raise-type-error 
	      who
	      (string-append also-expect "'sslv2-or-v3, 'sslv2, 'sslv3, or 'tls")
	      e)])))

  (define ssl-make-client-context
    (opt-lambda ([protocol-symbol default-encrypt])
      (let ([meth (encrypt->method 'ssl-make-client-context "" protocol-symbol #t)])
	(atomically ; so we reliably regsiter the finalizer
	 (let ([ctx (SSL_CTX_new meth)])
	   (check-valid ctx 'ssl-make-client-context "context creation")
	   (register-finalizer ctx (lambda (v) (SSL_CTX_free v)))
	   (make-ssl-client-context ctx))))))

  (define (get-context who context-or-encrypt-method)
    (if (ssl-client-context? context-or-encrypt-method)
	(ssl-client-context-ctx context-or-encrypt-method)
	(SSL_CTX_new (encrypt->method who "client context, " context-or-encrypt-method #t))))

  (define-struct mzssl (ssl i o r-bio w-bio pipe-r pipe-w buffer lock refcount))

  (define (mzssl-release mzssl)
    (call-with-semaphore
     (mzssl-lock mzssl)
     (lambda ()
       (set-mzssl-refcount! mzssl (sub1 (mzssl-refcount mzssl)))
       (when (zero? (mzssl-refcount mzssl))
	 (SSL_free (mzssl-ssl mzssl))))))

  (define (pump-input-once mzssl need-progress?)
    (let ([buffer (mzssl-buffer mzssl)]
	  [i (mzssl-i mzssl)]
	  [r-bio (mzssl-r-bio mzssl)])
      (let ([n ((if need-progress? read-bytes-avail! read-bytes-avail!*) buffer i)])
	(cond
	 [(eof-object? n) 
	  (BIO_set_mem_eof_return r-bio 1)
	  eof]
	 [(zero? n) 0]
	 [else (let ([m (BIO_write r-bio buffer n)])
		 (unless (= m n)
		   (error 'pump-input-once "couldn't write all bytes to BIO!"))
		 m)]))))

  (define (pump-output-once mzssl need-progress?)
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
		      (error 'pump-output-once "no output to pump!"))
		    #f)
		  (begin
		    (write-bytes buffer pipe-w 0 n)
		    (pump-output-once mzssl need-progress?))))
	    (let ([n ((if need-progress? write-bytes-avail write-bytes-avail*) buffer o 0 n)])
	      (if (zero? n)
		  #f
		  (begin
		    (port-commit-peeked n (port-progress-evt pipe-r) always-evt pipe-r)
		    #t)))))))

  (define (pump-output mzssl)
    (when (pump-output-once mzssl #f)
      (pump-output mzssl)))

  (define (make-ssl-input-port mzssl)
    (make-input-port/read-to-peek
     (format "SSL ~a" (object-name (mzssl-i mzssl)))
     ;; read proc:
     (letrec ([do-read
	       (lambda (buffer)
		 (pump-output mzssl)
		 (let ([n (SSL_read (mzssl-ssl mzssl) buffer (bytes-length buffer))])
		   (if (n . >= . 1)
		       n
		       (let ([err (SSL_get_error (mzssl-ssl mzssl) n)])
			 (cond
			  [(or (= err SSL_ERROR_ZERO_RETURN)
			       (and (= err SSL_ERROR_SYSCALL) (zero? n)))
			   ;; We hit the end-of-file
			   eof]
			  [(= err SSL_ERROR_WANT_READ)
			   (let ([n (pump-input-once mzssl #f)])
			     (if (eq? n 0)
				 (wrap-evt (mzssl-i mzssl) (lambda (x) 0))
				 (do-read buffer)))]
			  [(= err SSL_ERROR_WANT_WRITE)
			   (if (pump-output-once mzssl #f)
			       (do-read buffer)
			       (wrap-evt (mzssl-o mzssl) (lambda (x) 0)))]
			  [else
			   (error 'read-bytes "SSL read failed ~a"
				  (get-error-message (ERR_get_error)))])))))]
	      [lock-unavailable
	       (lambda () (wrap-evt (mzssl-lock mzssl) (lambda (x) 0)))])
       (lambda (buffer)
	 (call-with-semaphore
	  (mzssl-lock mzssl)
	  do-read
	  lock-unavailable
	  buffer)))
     ;; fast peek:
     #f
     ;; close proc:
     (lambda ()
       (mzssl-release mzssl))))

  (define (make-ssl-output-port mzssl)
    (make-output-port
     (format "SSL ~a" (object-name (mzssl-o mzssl)))
     (mzssl-o mzssl)
     ;; write proc:
     (letrec ([do-write
	       (lambda (buffer s e block-ok? enable-break?)
		 (pump-output mzssl)
		 (if (= s e)
		     0
		     (let ([n (SSL_write (mzssl-ssl mzssl) 
					 (if (zero? s)
					     buffer 
					     (subbytes buffer s e))
					 (- e s))])
		       (if (n . > . 0)
			   n
			   (let ([err (SSL_get_error (mzssl-ssl mzssl) n)])
			     (cond
			      [(= err SSL_ERROR_WANT_READ)
			       (let ([n (pump-input-once mzssl #f)])
				 (if (eq? n 0)
				     (wrap-evt (mzssl-i mzssl) (lambda (x) #f))
				     (do-write buffer s e block-ok? enable-break?)))]
			      [(= err SSL_ERROR_WANT_WRITE)
			       (if (pump-output-once mzssl #f)
				   (do-write buffer s e block-ok? enable-break?)
				   (wrap-evt (mzssl-o mzssl) (lambda (x) #f)))]
			      [else
			       (error 'read-bytes "SSL read failed ~a"
				      (get-error-message (ERR_get_error)))]))))))]
	      [lock-unavailable
	       (lambda () (wrap-evt (mzssl-lock mzssl) (lambda (x) #f)))])
       (lambda (buffer s e block-ok? enable-break?)
	 (call-with-semaphore
	  (mzssl-lock mzssl)
	  do-write
	  lock-unavailable
	  buffer s e block-ok? enable-break?)))
     ;; close proc:
     (lambda ()
       ;; issue shutdown (i.e., EOF on read end)
       (let loop ()
	 (pump-output mzssl)
	 (let ([n (SSL_shutdown (mzssl-ssl mzssl))])
	   (unless (= n 1)
	     (let ([err (SSL_get_error (mzssl-ssl mzssl) n)])
	       (cond
		[(= err SSL_ERROR_WANT_READ)
		 (pump-input-once mzssl #t)
		 (loop)]
		[(= err SSL_ERROR_WANT_WRITE)
		 (pump-output-once mzssl #t)
		 (loop)]
		[else
		 (error 'read-bytes "SSL shutdown failed ~a"
			(get-error-message (ERR_get_error)))])))))
       (mzssl-release mzssl))))

  (define (ports->ssl-ports i o context-or-encrypt-method connect/accept close?)
    (let ([who 'input-port->ssl-input-port])
      (unless (input-port? i)
	(raise-type-error who "input port" i))
      (unless (output-port? o)
	(raise-type-error who "output port" o))
      (let ([ctx (get-context who context-or-encrypt-method)])
	(check-valid ctx who "context creation")
	(with-failure
	 (lambda () (when (and ctx
			       (symbol? context-or-encrypt-method))
		      (SSL_CTX_free ctx)))
	 (let ([connect? (case connect/accept
			   [(connect) #t]
			   [(accept) #f]
			   [else
			    (raise-type-error who "'connect or 'accept" 
					      connect/accept)])]
	       [r-bio (BIO_new (BIO_s_mem))]
	       [w-bio (BIO_new (BIO_s_mem))]
	       [free-bio? #t])
	   (with-failure
	    (lambda () (when free-bio?
			 (BIO_free r-bio)
			 (BIO_free w-bio)))
	    (let ([ssl (SSL_new ctx)])
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
	       ;; connect/accept:
	       (let-values ([(buffer) (make-bytes 512)]
			    [(pipe-r pipe-w) (make-pipe)])
		 (let ([mzssl (make-mzssl ssl i o r-bio w-bio pipe-r pipe-w buffer (make-semaphore 1) 2)])
		   (let loop ()
		     (let ([status (if connect?
				       (SSL_connect ssl)
				       (SSL_accept ssl))])
		       (pump-output mzssl)
		       (when (status . < . 1)
			 (let ([err (SSL_get_error ssl status)])
			   (cond
			    [(= err SSL_ERROR_WANT_READ)
			     (let ([n (pump-input-once mzssl #t)])
			       (when (eof-object? n)
				 (error who "~a failed (input terminated prematurely)"
					(if connect? "connect" "accept"))))
			     (loop)]
			    [(= err SSL_ERROR_WANT_WRITE)
			     (pump-output-once mzssl #t)
			     (loop)]
			    [else
			     (error who "~a failed ~a" 
				    (if connect? "connect" "accept")
				    (get-error-message (ERR_get_error)))])))))
		   ;; Connection complete; make ports
		   (values (make-ssl-input-port mzssl)
			   (make-ssl-output-port mzssl))))))))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Initialization

  (SSL_library_init)
  (SSL_load_error_strings)

  )

  

  