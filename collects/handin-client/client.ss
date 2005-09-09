
(module client mzscheme
  (require (lib "mzssl.ss" "openssl"))

  (provide handin-connect
	   submit-assignment
	   submit-addition
	   submit-password-change)

  (define-struct handin (r w))

  (define (handin-connect server port pem)
    (let ([ctx (ssl-make-client-context)])
      (ssl-set-verify! ctx #t)
      (ssl-load-verify-root-certificates! ctx pem)
      (let-values ([(r w) (ssl-connect server port ctx)])
	;; Sanity check: server sends "handin", first:
	(let ([s (read-bytes 6 r)])
	  (unless (equal? #"handin" s)
	    (error 'handin-connect "bad handshake from server: ~e" s)))
	;; Tell server protocol = 'original:
	(fprintf w "original\n")
	(flush-output w)
	;; One more sanity check: server recognizes protocol:
	(let ([s (read r)])
	  (unless (eq? s 'original)
	    (error 'handin-connect "bad protocol from server: ~e" s)))
	;; Return connection and list of active assignments:
	(values (make-handin r w)
		(let ([v (read r)])
		  (unless (and (list? v)
			       (andmap string? v))
		    (error 'handin-connect "failed to get active-assignment list from server"))
		  v)))))

  (define (submit-assignment h username passwd assignment content on-commit)
    (let ([r (handin-r h)]
	  [w (handin-w h)])
      (fprintf w "~s ~s ~s\n" username passwd assignment)
      (flush-output w)
      (let ([v (read r)])
	(unless (eq? v 'ok)
	  (error 'handin-connect "login error: ~a" v)))
      (fprintf w "~s\n" (bytes-length content))
      (flush-output w)
      (let ([v (read r)])
	(unless (eq? v 'go)
	  (error 'handin-connect "upload error: ~a" v)))
      (fprintf w "$")
      (display content w)
      (flush-output w)
      (let ([v (read r)])
	(unless (eq? v 'confirm)
	  (error 'handin-connect "submit error: ~a" v)))
      (on-commit)
      (fprintf w "check\n")
      (flush-output w)
      (let ([result-msg 
	     (let ([v (read r)])
	       (cond
		[(eq? v 'done) #f]
		[(and (pair? v) (eq? (car v) 'result))
		 (cadr v)]
		[else
		 (error 'handin-connect "commit probably unsucccesful: ~e" v)]))])
	(close-input-port r)
	(close-output-port w)
	result-msg)))


  (define (submit-addition h username full-name id email passwd)
    (let ([r (handin-r h)]
	  [w (handin-w h)])
      (fprintf w "~s create ~s ~s ~s ~s~n" username full-name id email passwd)
      (let ([v (read r)])
	(unless (eq? v 'ok)
	  (error 'handin-connect "update error: ~a" v)))
      (close-input-port r)
      (close-output-port w)))

  (define (submit-password-change h username old-passwd new-passwd)
    (let ([r (handin-r h)]
	  [w (handin-w h)])
      (fprintf w "~s ~s change ~s~n" username old-passwd new-passwd)
      (let ([v (read r)])
	(unless (eq? v 'ok)
	  (error 'handin-connect "update error: ~a" v)))
      (close-input-port r)
      (close-output-port w))))
