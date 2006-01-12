(module client mzscheme
  (require (lib "mzssl.ss" "openssl"))

  (provide handin-connect
	   handin-disconnect
	   retrieve-user-fields
	   retrieve-active-assignments
	   submit-assignment
	   retrieve-assignment
	   submit-addition
	   submit-info-change
	   retrieve-user-info)

  (define-struct handin (r w))

  (define (write+flush port . xs)
    (for-each (lambda (x) (write x port) (newline port)) xs)
    (flush-output port))

  (define (close-handin-ports h)
    (close-input-port (handin-r h))
    (close-output-port (handin-w h)))

  (define (wait-for-ok r who . reader)
    (let ([v (if (pair? reader) ((car reader)) (read r))])
      (unless (eq? v 'ok) (error 'handin-connect "~a error: ~a" who v))))

  (define (handin-connect server port pem)
    (let ([ctx (ssl-make-client-context)])
      (ssl-set-verify! ctx #t)
      (ssl-load-verify-root-certificates! ctx pem)
      (let-values ([(r w) (ssl-connect server port ctx)])
	;; Sanity check: server sends "handin", first:
	(let ([s (read-bytes 6 r)])
	  (unless (equal? #"handin" s)
	    (error 'handin-connect "bad handshake from server: ~e" s)))
	;; Tell server protocol = 'ver1:
	(write+flush w 'ver1)
	;; One more sanity check: server recognizes protocol:
	(let ([s (read r)])
	  (unless (eq? s 'ver1)
	    (error 'handin-connect "bad protocol from server: ~e" s)))
	;; Return connection:
        (make-handin r w))))

  (define (handin-disconnect h)
    (write+flush (handin-w h) 'bye)
    (close-handin-ports h))

  (define (retrieve-user-fields h)
    (let ([r (handin-r h)] [w (handin-w h)])
      (write+flush w 'get-user-fields 'bye)
      (let ([v (read r)])
        (unless (and (list? v) (andmap string? v))
          (error 'handin-connect
                 "failed to get user-fields list from server"))
        (wait-for-ok r "get-user-fields")
        (close-handin-ports h)
        v)))

  (define (retrieve-active-assignments h)
    (let ([r (handin-r h)] [w (handin-w h)])
      (write+flush w 'get-active-assignments)
      (let ([v (read r)])
        (unless (and (list? v) (andmap string? v))
          (error 'handin-connect
                 "failed to get active-assignment list from server"))
        v)))

  (define (submit-assignment h username passwd assignment content
                             on-commit message message-final message-box)
    (let ([r (handin-r h)] [w (handin-w h)])
      (define (read/message)
        (let ([v (read r)])
          (case v
            [(message) (message (read r)) (read/message)]
            [(message-final) (message-final (read r)) (read/message)]
            [(message-box)
             (write+flush w (message-box (read r) (read r))) (read/message)]
            [else v])))
      (write+flush w
        'set 'username/s username
        'set 'password   passwd
        'set 'assignment assignment
        'save-submission)
      (wait-for-ok r "login")
      (write+flush w (bytes-length content))
      (let ([v (read r)])
	(unless (eq? v 'go)
	  (error 'handin-connect "upload error: ~a" v)))
      (display "$" w)
      (display content w)
      (flush-output w)
      ;; during processing, we're waiting for 'confirm, in the meanwhile, we
      ;; can get a 'message or 'message-box to show -- after 'message we expect
      ;; a string to show using the `messenge' argument, and after 'message-box
      ;; we expect a string and a style-list to be used with `message-box' and
      ;; the resulting value written back
      (let ([v (read/message)])
        (unless (eq? 'confirm v)
          (error (format "submit error: ~a" v))))
      (on-commit)
      (write+flush w 'check)
      (wait-for-ok r "commit" read/message)
      (close-handin-ports h)))

  (define (retrieve-assignment h username passwd assignment)
    (let ([r (handin-r h)] [w (handin-w h)])
      (write+flush w
        'set 'username/s username
        'set 'password   passwd
        'set 'assignment assignment
        'get-submission)
      (let ([len (read r)])
        (unless (and (number? len) (integer? len) (positive? len))
          (error 'handin-connect "bad response from server: ~a" len))
        (let ([buf (begin (regexp-match #rx"[$]" r) (read-bytes len r))])
          (wait-for-ok r "get-submission")
          (close-handin-ports h)
          buf))))

  (define (submit-addition h username passwd user-fields)
    (let ([r (handin-r h)] [w (handin-w h)])
      (write+flush w
        'set 'username/s  username
        'set 'password    passwd
        'set 'user-fields user-fields
        'create-user)
      (wait-for-ok r "create-user")
      (close-handin-ports h)))

  (define (submit-info-change h username old-passwd new-passwd user-fields)
    (let ([r (handin-r h)]
	  [w (handin-w h)])
      (write+flush w
        'set 'username/s   username
        'set 'password     old-passwd
        'set 'new-password new-passwd
        'set 'user-fields  user-fields
        'change-user-info)
      (wait-for-ok r "change-user-info")
      (close-handin-ports h)))

  (define (retrieve-user-info h username passwd)
    (let ([r (handin-r h)] [w (handin-w h)])
      (write+flush w
        'set 'username/s username
        'set 'password   passwd
        'get-user-info 'bye)
      (let ([v (read r)])
        (unless (and (list? v) (andmap string? v))
          (error 'handin-connect "failed to get user-info list from server"))
        (wait-for-ok r "get-user-info")
        (close-handin-ports h)
        v)))

  )
