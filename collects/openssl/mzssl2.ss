
;; This is a re-implementation of "mzssl.c" using `(lib "foreign.ss")'.
;; It will soon replace "mzssl.c".

;; Warn clients: even when a (non-blocking) write fails to write all
;;   the data, the stream is committed to writing the given data in
;;   the future. (This requirement comes from the SSL library.)

(module mzssl2 mzscheme
  (require (lib "foreign.ss")
	   (lib "port.ss")
	   (lib "etc.ss"))

  (provide ssl-available?

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

	   ports->ssl-ports

	   ssl-listen
	   ssl-close
	   ssl-accept
	   ssl-accept/enable-break
	   ssl-connect
	   ssl-connect/enable-break)

  (unsafe!)

  (define libssl (with-handlers ([exn:fail? (lambda (x) #f)])
		   (ffi-lib "libssl")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SSL bindings and constants

  (define-syntax define-define-X
    (syntax-rules ()
      [(_ id chk lib)
       (define-syntax (id stx)
	 (syntax-case stx ()
	   [(_ id type)
	    (with-syntax ([str (symbol->string (syntax-e #'id))])
	      #'(define id
		  (and chk
		       (get-ffi-obj str lib (_fun . type)))))]))]))

  (define-define-X define-ssl libssl libssl)
  (define-define-X define-mzscheme #t #f)

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

  (define-ssl SSL_CTX_set_verify (_SSL_CTX* _int _pointer -> _void))
  (define-ssl SSL_CTX_use_certificate_chain_file (_SSL_CTX* _bytes -> _int))
  (define-ssl SSL_CTX_load_verify_locations (_SSL_CTX* _bytes -> _int))
  (define-ssl SSL_CTX_set_client_CA_list (_SSL_CTX* _X509_NAME* -> _int))
  (define-ssl SSL_CTX_use_RSAPrivateKey_file (_SSL_CTX* _bytes _int -> _int))
  (define-ssl SSL_CTX_use_PrivateKey_file (_SSL_CTX* _bytes _int -> _int))
  (define-ssl SSL_load_client_CA_file (_bytes -> _X509_NAME*))

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

  (define SSL_FILETYPE_PEM 1)
  (define SSL_FILETYPE_ASN1 2)

  (define SSL_VERIFY_NONE #x00)
  (define SSL_VERIFY_PEER #x01)
  (define SSL_VERIFY_FAIL_IF_NO_PEER_CERT #x02)

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
  ;; Structs

  (define-struct ssl-context (ctx))
  (define-struct (ssl-client-context ssl-context) ())
  (define-struct (ssl-server-context ssl-context) ())

  (define-struct ssl-listener (l mzctx))

  ;; internal:
  (define-struct mzssl (ssl i o r-bio w-bio pipe-r pipe-w buffer lock refcount))

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
       [else (raise-type-error 
	      who
	      (string-append also-expect "'sslv2-or-v3, 'sslv2, 'sslv3, or 'tls")
	      e)])))

  (define make-context
    (opt-lambda (who protocol-symbol also-expected client?)
      (let ([meth (encrypt->method who also-expected protocol-symbol client?)])
	(atomically ; so we reliably register the finalizer
	 (let ([ctx (SSL_CTX_new meth)])
	   (check-valid ctx who "context creation")
	   (register-finalizer ctx (lambda (v) (SSL_CTX_free v)))
	   ((if client? make-ssl-client-context make-ssl-server-context) ctx))))))

  (define ssl-make-client-context
    (opt-lambda ([protocol-symbol default-encrypt])
      (make-context 'ssl-make-client-context protocol-symbol "" #t)))

  (define ssl-make-server-context
    (opt-lambda ([protocol-symbol default-encrypt])
      (make-context 'ssl-make-server-context protocol-symbol "" #f)))

  (define (get-context who context-or-encrypt-method client?)
    (if (ssl-context? context-or-encrypt-method)
	(ssl-context-ctx context-or-encrypt-method)
	(SSL_CTX_new (encrypt->method who "context" context-or-encrypt-method client?))))

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
		   (path->complete-path (expand-path pathname)
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
		  SSL_CTX_load_verify_locations
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

  (define ssl-load-private-key!
    (opt-lambda (ssl-context-or-listener pathname [rsa? #t] [asn1? #f])
      (ssl-load-... 'ssl-load-private-key! 
		    (lambda (ctx path)
		      ((if rsa?
			   SSL_CTX_use_RSAPrivateKey_file
			   SSL_CTX_use_PrivateKey_file)
		       ctx path
		       (if asn1?
			   SSL_FILETYPE_ASN1
			   SSL_FILETYPE_PEM)))
		    ssl-context-or-listener pathname)))

  (define (ssl-set-verify! ssl-context-or-listener on?)
    (let ([ctx (get-context/listener 'ssl-set-verify!
				     ssl-context-or-listener)])
      (SSL_CTX_set_verify ctx
			  (if on?
			      (bitwise-ior SSL_VERIFY_PEER 
					   SSL_VERIFY_FAIL_IF_NO_PEER_CERT)
			      SSL_VERIFY_NONE)
			  #f)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SSL ports

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
		      (error 'pump-output-once "no output to pump!"))
		    #f)
		  (begin
		    (write-bytes buffer pipe-w 0 n)
		    (pump-output-once mzssl need-progress?
				      output-blocked-result))))
	    (let ([n ((if need-progress? write-bytes-avail write-bytes-avail*) buffer o 0 n)])
	      (if (zero? n)
		  output-blocked-result
		  (begin
		    (port-commit-peeked n (port-progress-evt pipe-r) always-evt pipe-r)
		    #t)))))))

  (define (pump-output mzssl)
    (when (pump-output-once mzssl #f #f)
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
			   (if (pump-output-once mzssl #f #f)
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
    ;; Need a consistent buffer to use with SSL_write
    ;;  across calls to the port's write function.
    (let ([xfer-buffer (make-bytes 512)])
      (make-output-port
       (format "SSL ~a" (object-name (mzssl-o mzssl)))
       (mzssl-o mzssl)
       ;; write proc:
       (letrec ([do-write
		 (lambda (len block-ok? enable-break?)
		   (pump-output mzssl)
		   (if (zero? len)
		       ;; Flush request; all data is in the the SSL
		       ;;  stream, but how do we know that it's gone
		       ;;  through the ports (which may involve both
		       ;;  output and input)? It seems that making
		       ;;  sure all output is gone is sufficient.
		       ;;  We've already pumped output, but maybe some
		       ;;  is stuck in the bio...
		       (parameterize-break 
			enable-break?
			(let loop ()
			  (flush-output (mzssl-o mzssl))
			  (when (pump-output-once mzssl #f #t)
			    (loop)))
			0)
		       ;; Write request; even if blocking is ok, we treat
		       ;;  it as non-blocking and let MzScheme handle blocking
		       (let ([n (SSL_write (mzssl-ssl mzssl) xfer-buffer len)])
			 (if (n . > . 0)
			     n
			     (let ([err (SSL_get_error (mzssl-ssl mzssl) n)])
			       (cond
				[(= err SSL_ERROR_WANT_READ)
				 (let ([n (pump-input-once mzssl #f)])
				   (if (eq? n 0)
				       (wrap-evt (mzssl-i mzssl) (lambda (x) #f))
				       (do-write len block-ok? enable-break?)))]
				[(= err SSL_ERROR_WANT_WRITE)
				 (if (pump-output-once mzssl #f #f)
				     (do-write len block-ok? enable-break?)
				     (wrap-evt (mzssl-o mzssl) (lambda (x) #f)))]
				[else
				 (error 'read-bytes "SSL read failed ~a"
					(get-error-message (ERR_get_error)))]))))))]
		[top-write
		 (lambda (buffer s e block-ok? enable-break?)
		   (bytes-copy! xfer-buffer 0 buffer s e)
		   (do-write (- e s) block-ok? enable-break?))]
		[lock-unavailable
		 (lambda () (wrap-evt (mzssl-lock mzssl) (lambda (x) #f)))])
	 (lambda (buffer s e block-ok? enable-break?)
	   (call-with-semaphore
	    (mzssl-lock mzssl)
	    top-write
	    lock-unavailable
	    buffer s e block-ok? enable-break?)))
       ;; close proc:
       (lambda ()
	 ;; issue shutdown (i.e., EOF on read end)
	 (let loop ([cnt 1])
	   (pump-output mzssl)
	   (let ([n (SSL_shutdown (mzssl-ssl mzssl))])
	     (if (= n 0)
		 ;; 0 seems to be the result in many cases because the socket
		 ;; is non-blocking, and then neither of the WANTs is returned.
		 ;; We address this by simply trying 10 times and then giving
		 ;; up. The two-step shutdown is optional, anyway.
		 (unless (cnt . >= . 10)
		   (loop (add1 cnt)))
		 (unless (= n 1)
		   (let ([err (SSL_get_error (mzssl-ssl mzssl) n)])
		     (cond
		      [(= err SSL_ERROR_WANT_READ)
		       (pump-input-once mzssl #t)
		       (loop)]
		      [(= err SSL_ERROR_WANT_WRITE)
		       (pump-output-once mzssl #t #f)
		       (loop)]
		      [else
		       (error 'read-bytes "SSL shutdown failed ~a"
			      (get-error-message (ERR_get_error)))]))))))
	 (mzssl-release mzssl)))))

  (define (ports->ssl-ports i o context-or-encrypt-method connect/accept close?)
    (wrap-ports 'port->ssl-ports i o context-or-encrypt-method connect/accept close?))

  (define (wrap-ports who i o context-or-encrypt-method connect/accept close?)
    (unless (input-port? i)
      (raise-type-error who "input port" i))
    (unless (output-port? o)
      (raise-type-error who "output port" o))
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
			  (raise-type-error who "'connect or 'accept" 
					    connect/accept)])]
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
	    (error who
		   "'~a mode requires a ~a context, given: ~e"
		   (if connect? 'connect 'accept)
		   (if connect? "client" "server")
		   context-or-encrypt-method))
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
			   (pump-output-once mzssl #t #f)
			   (loop)]
			  [else
			   (error who "~a failed ~a" 
				  (if connect? "connect" "accept")
				  (get-error-message (ERR_get_error)))])))))
		 ;; Connection complete; make ports
		 (values (make-ssl-input-port mzssl)
			 (make-ssl-output-port mzssl)))))))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SSL listen

  (define ssl-listen
    (opt-lambda (port-k [queue-k 5] [reuse? #f] [hostname-or-#f #f] [protocol-symbol-or-context default-encrypt])
      (let ([ctx (cond
		  [(ssl-server-context? protocol-symbol-or-context) protocol-symbol-or-context]
		  [else (make-context 'ssl-listen protocol-symbol-or-context "server context, " #f)])])
	(let ([l (tcp-listen port-k queue-k reuse? hostname-or-#f)])
	  (make-ssl-listener l ctx)))))

  (define (ssl-close l)
    (unless (ssl-listener? l)
      (raise-type-error 'ssl-close "SSL listener" l))
    (tcp-close (ssl-listener-l l)))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SSL accept

  (define (do-ssl-accept who tcp-accept ssl-listener)
    (let-values ([(i o) (tcp-accept (ssl-listener-l ssl-listener))])
      (wrap-ports who i o (ssl-listener-mzctx ssl-listener) 'accept #t)))

  (define (ssl-accept ssl-listener)
    (do-ssl-accept 'ssl-accept tcp-accept ssl-listener))

  (define (ssl-accept/enable-break ssl-listener)
    (do-ssl-accept 'ssl-accept/enable-break tcp-accept/enable-break ssl-listener))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SSL connect

  (define (do-ssl-connect who tcp-connect hostname port-k client-context-or-protocol-symbol)
    (let-values ([(i o) (tcp-connect hostname port-k)])
      (wrap-ports who i o client-context-or-protocol-symbol 'connect #t)))

  (define ssl-connect 
    (opt-lambda (hostname port-k [client-context-or-protocol-symbol default-encrypt])
      (do-ssl-connect 'ssl-connect tcp-connect hostname port-k 
		      client-context-or-protocol-symbol)))

  (define ssl-connect/enable-break
    (opt-lambda (hostname port-k [client-context-or-protocol-symbol default-encrypt])
      (do-ssl-connect 'ssl-connect/enable-break tcp-connect/enable-break hostname port-k
		      client-context-or-protocol-symbol)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Initialization

  (define ssl-available? (and libssl #t))

  (when ssl-available?
    (SSL_library_init)
    (SSL_load_error_strings))

  )
