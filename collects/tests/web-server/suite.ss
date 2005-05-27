; HOW TO RUN:
; Call broken? with a port number (greater than 1024 unless you're root)
; i.e. (broken? 8080)
; It should return #f if it's not broken, otherwise it returns a string explaining the brokenness.

; note - should be run on multiple platforms

; more-here - test flushing pending requests to a terminated servlet

(module suite mzscheme
  (provide broken? broken?/remote start-server files-broken? authentication-broken?
           normal-servlets-broken? errors-broken?)
  (require (lib "process.ss")
           (lib "etc.ss")
           "my-url.ss"
           (lib "base64.ss" "net")
           (lib "xml.ss" "xml")
           (lib "channel.ss" "web-server"))

  (define myprint void)
  
  ; header-pattern = (listof (cons sym (+ regexp str)))
  
  (error-print-width 800)
  
  (define test-directory (build-path (collection-path "tests") "web-server"))
  (define web-root (build-path test-directory "web-root"))
  ;(define web-root (collection-path "web-server" "default-web-root"))
  (define answers-directory (build-path test-directory "answers"))
  (define TEST-IP "127.0.0.1")
  
  (print-struct #t)

  ;; broken?/remote: nat -> (union falses str)
  ;; same as broken? but connect to a running web-server
  ;; rather than starting a new one
  (define (broken?/remote port)
    (or (files-broken? port)
        (authentication-broken? port)
        (normal-servlets-broken? port)
        (extended-servlets-broken? port)
        (errors-broken? port)
        (timeouts-broken? port)))
  
  ; broken? : nat -> (+ false str)
  ;; the cadilac broken? function, starts the server automatically.
  (define (broken? port)
    (or (channels-broken?)
        (let ([server (start-server port)])
	  (dynamic-wind
	      void
	      (lambda ()
		(or (and (not (subprocess? server)) server)
		    (files-broken? port)
		    (authentication-broken? port)
                    (normal-servlets-broken? port)
		    (extended-servlets-broken? port)
		    (errors-broken? port)
		    (timeouts-broken? port)))
	      (lambda () (kill-subprocess server))))))

  ; channels-broken? : -> (U str #f)
  ; more here - stress test for synchronization defects
  (define (channels-broken?)
    (let ([c (make-async-channel)]
          [v (gensym)])
      (async-channel-put c v)
      (if (eq? v (async-channel-get c))
          (or (let ([*x* #f])
                (async-channel-get-available
                 c
                 (lambda (y) (set! *x* "channel: not empty")))
                *x*)
              (let ([*x* "channel: nothing available"])
                (async-channel-put c v)
                (async-channel-get-available c (lambda (y) (set! *x* #f)))
                *x*)
              (let/ec k
                (async-channel-try-get c (lambda () (k #f)))
                "channel: removed item from empty channel")
              (let/ec k
                (let ([x (gensym)])
                  (async-channel-put c x)
                  (async-channel-try-get
                   c
                   (lambda () (k "channel: fail to remove x")))
                  #f)))
          "channel: put/get mismatch")))
  
  ; start-server : nat -> (U str subprocess)
  (define (start-server port)
    (let-values ([(mz-subprocess mz-out mz-in mz-err)
                  (subprocess #f #f #f
                              (find-executable-path "web-server" #f)
                              "-p" (number->string port)
                              "-f" (path->string (build-path test-directory "configuration-table")) )])
      (sleep 5)
      (if (not (eq? 'running (subprocess-status mz-subprocess)))
          (format "server mzscheme isn't running:~n~s"
                  ; just print the first 20 lines of junk or so
                  (read-string 1600 mz-err))
          (if (char-ready? mz-err)
              (format "server printed error: ~s" (read-line mz-err))
              mz-subprocess))))
  
  ; kill-subprocess : subprocess -> void
  (define (kill-subprocess p)
    (let ([pid (subprocess-pid p)]
          [kill-path (find-executable-path "kill" #f)])
      (unless (or (zero? pid) (not kill-path))
        (let-values ([(kill-subprocess out in err)
                      (subprocess #f #f #f kill-path (number->string pid))])
          (close-input-port out)
          (close-output-port in)
          (close-input-port err)
          (subprocess-wait kill-subprocess)))))
  
  ; wont-start? : nat -> (+ false str)
  ; effect: tries to start the server
  '(define (wont-start? port)
     (with-handlers ([void (lambda (exn) (format "start-server: exn = ~a" exn))])
       (let*-values ([(mz-subprocess mz-out mz-in mz-err)
                      (subprocess #f #f #f
                                  (find-executable-path "web-server" #f)
                                  "-p" (number->string port))]
                     [(send-command)
                      ; String -> Void
                      (lambda (command)
                        (write command mz-in)
                        (newline mz-in))])
         ;(send-command `(current-directory ,web-root))
         (send-command `(serve ,port))
         (sleep 5)
         (if (not (eq? 'running (subprocess-status mz-subprocess)))
             (format "server mzscheme isn't running:~n~s"
                     ; just print the first 20 lines of junk or so
                     (read-string 1600 mz-err))
             (if (char-ready? mz-err)
                 (format "server printed error: ~s" (read-line mz-err))
                 #f)))))
  
  (define date-regexp "^(Sun|Mon|Tue|Wed|Thu|Fri|Sat), [0-9]* (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) [0-9]* [0-9][0-9]:[0-9][0-9]:[0-9][0-9] GMT$")
  
  (define usual-headers
    `((date ,date-regexp)
      (Last-Modified ,date-regexp)
      (server "^PLT Scheme$")
      (content-type "^text/html$")
      (connection "^close$")))
  
  (define jpeg-headers
    `((date ,date-regexp)
      (Last-Modified ,date-regexp)
      (server "^PLT Scheme$")
      (content-type "^image/jpeg$")
      (connection "^close$")))
 
  ; content-length-header : str -> (list sym str)
  (define (content-length-header file-path)
    (myprint "   file-path = ~s~n" file-path)
    `(content-length ,(format "^~a$" (file-size file-path))))
  
  ; files-broken? : nat -> (+ false string)
  (define (files-broken? port)
    (myprint "files-broken?~n")
    (with-handlers ([void (lambda (exn) (format "test-files: error starting up ~a" exn))])
      (let* ([file-path (build-path web-root "htdocs" "index.html")]
	     [implicit-url (local-url port "")]
	     [explicit-url (combine-url/relative implicit-url "index.html")]
	     [pattern (append usual-headers (list (content-length-header file-path)))]

             ;; jpeg tests
             ;; for PR#6302
             [picture1-path (build-path web-root "htdocs" "me.jpg")]
             [picture1-url (combine-url/relative implicit-url "me.jpg")]
             [picture1-pattern (append jpeg-headers (list (content-length-header picture1-path)))]
             
             ;; reproduces PR#6302 b-cuz me2.JPG has upper-case extension
             [picture2-path (build-path web-root "htdocs" "me2.JPG")]
             [picture2-url (combine-url/relative implicit-url "me2.JPG")]
             [picture2-pattern (append jpeg-headers (list (content-length-header picture2-path)))]
             )
	(or (problem-with-url? file-path pattern explicit-url)
	    (problem-with-url? file-path pattern implicit-url)
            (problem-with-url? picture1-path picture1-pattern picture1-url)
            (problem-with-url? picture2-path picture2-pattern picture2-url)
            ))))
  
  ; problem-with-url? : str header-pattern url [(listof str)] -> (U false str)
  (define problem-with-url?
    (opt-lambda (file-path header-match url [extra-headers null])
      (delimit-resources
       (lambda ()
	 (with-handlers ([void (lambda (exn) (format "test-url: url = ~a~n  ~a~n" url exn))])
	   (call-with-input-file file-path
	     (lambda (file-input)
	       (let* ([http-port (get-impure-port url extra-headers)]
		      [headers (purify-port http-port)])
		 (error-add (format "problem-with-url: ~s: " (url->string url))
                            (or (mime-headers-problem? headers header-match)
                                (input-port-diff http-port file-input)))))))))))
  
  (define (print-port i-port)
    (printf "inside print-port~n")
    (let loop ([l (read-line i-port)])
      (unless (eof-object? l)
        (printf "~a~n" l)
        (loop (read-line i-port)))))
  
  ; : url header-pattern regexp -> (U false str)
  (define (broken-url-regexp? to-test header-match expected)
    (delimit-resources
     (lambda ()
       (with-handlers ([void (lambda (exn) (format "exception for url ~e~n~e" to-test exn))])
         (let* ([http-port (get-impure-port to-test)]
                [headers (purify-port http-port)])
           (error-add (format "url did not match: ~e: " (url->string to-test))
                      (or (mime-headers-problem? headers header-match)
                          (regexp-match expected http-port))))))))
  
  ; delimit-resources : (-> a) -> a
  (define (delimit-resources thunk)
    (let ([cust (make-custodian)]
	  [old-cust (current-custodian)])
      (dynamic-wind (lambda () (current-custodian cust))
		    thunk
		    (lambda ()
		      (current-custodian old-cust)
		      (custodian-shutdown-all cust)))))
  
  ; authentication-broken? : nat -> (+ false str)
  (define (authentication-broken? port)
    (myprint "authentication-broken?~n")
    (let* ([forbidden-file-path (build-path web-root "conf" "forbidden.html")]
	   [okay-file-path (build-path web-root "htdocs" "secret" "index.html")]
	   [forbidden-content-length (content-length-header forbidden-file-path)]
	   [okay-content-length (content-length-header okay-file-path)]
	   [auth-header `(www-authenticate "^Basic realm=\"secret stuff\"$")]
	   [forbidden-headers (append usual-headers (list forbidden-content-length auth-header))]
	   [okay-headers (append usual-headers (list okay-content-length))]
;	   [authorization (list (format "authorization: Basic ~a" (base64-encode "bubba:bbq")))]
           [authorization (list (bytes->string/utf-8 (bytes-append #"authorization: Basic " (base64-encode #"bubba:bbq"))))]
           )
      (or (problem-with-url? forbidden-file-path
			     forbidden-headers
			     (local-url port "secret/"))
	  (problem-with-url? okay-file-path
			     okay-headers
			     (local-url port "secret/")
			     authorization)
	  (problem-with-url? forbidden-file-path
			     forbidden-headers
			     (local-url port "secret/index.html"))
	  (problem-with-url? okay-file-path
			     okay-headers
			     (local-url port "secret/index.html")
			     authorization))))
  
  ; normal-servlets-broken? : Nat -> (+ false str)
  (define (normal-servlets-broken? port)
    (myprint "normal-servlets-broken?~n")
    (let* ([local-test-url
            (lambda (path)
              (local-url port (string-append "servlets/tests/" path)))]
           [broken-url?
            (lambda (answer prog)
              (let ([answer-1 (build-path answers-directory answer)])
                (problem-with-url? answer-1
                                   (append usual-headers (list (content-length-header answer-1)))
                                   (local-test-url prog))))])
      (or (broken-url? "test.servlet-1" "test.ss")
	  (broken-url? "test.servlet-2" "test.ss?a=b&see=def")
	  (problem-with-url? (build-path answers-directory "incremental")
                             usual-headers
                             (local-url port "servlets/tests/incremental.ss"))
          ; more here - test chunked version
	  ;(problem-with-url? (build-path answers-directory "incremental")
          ;                   `(,(car usual-headers)
          ;                     ,(cadr usual-headers)
          ;                     ,(caddr usual-headers)
          ;                     (Transfer-Encoding "^chunked$"))
          ;                   (local-url port "servlets/tests/incremental.ss"))
          (let ([string-answer (build-path answers-directory "mime-servlet")])
	    (problem-with-url? string-answer
			       `(,(car usual-headers)
				 ,(cadr usual-headers)
				 ,(caddr usual-headers)
				 (content-type "^text/uber-format$")
				 (connection "^close$")
				 ,(content-length-header string-answer))
			       (local-url port "servlets/tests/mime.ss")))
          (broken-url? "a-module" "a-module.ss")
          (broken-url? "b-module" "b-module.ss?texan=big-hat")
          ;(broken-url? "suspended-module" "suspended-module.ss") ; k-id is random
          (broken-url-regexp? (local-test-url "suspended-module.ss")
                              (append usual-headers (list `(content-length "[0-9]*")))
                              suspended-module-regexp))))
  
  (define suspended-module-regexp
    (regexp "<html><head><meta http-equiv=\"Pragma\" content=\"no-cache\" /><meta http-equiv=\"expires\" content=\"-1\" /><title>What is your name?</title></head><body bgcolor=\"white\"><form action=\"/servlets/tests/suspended-module.ss;id[0-9]*[*]k1-[0-9]*\" method=\"post\">What is your name?<input type=\"text\" name=\"name\" /></form></body></html>"))
  
  ; extended-servlets-broken? : Nat -> (+ false str)
  (define (extended-servlets-broken? port)
    #f)

  ; : (list sym str)
  (define generic-content-length-header
    `(content-length "^[0-9][0-9]*$"))
  
  ; errors-broken? : Nat -> (+ false str)
  ; tests file-not-found, servlet-error, and protocol-error.  The access-denied error is under authentication-broken?
  (define (errors-broken? port)
    (printf "errors-broken?:~n")
    (let* ([not-found-path (build-path web-root "conf" "not-found.html")]
	   [not-found-headers (append usual-headers (list (content-length-header not-found-path)))]
	   [non-unit-headers (append usual-headers (list generic-content-length-header))]
	   [servlet-error-path (build-path web-root "conf" "servlet-error.html")]
           [servlet-error-headers (append usual-headers (list (content-length-header servlet-error-path)))])
      (or (problem-with-url? not-found-path
			     not-found-headers
			     (local-url port "/conf/some-file-name-that-is-not-there.hmtl"))
          (problem-with-url? not-found-path
			     not-found-headers
			     (local-url port "some-file-name-that-is-not-there.hmtl"))
	  (problem-with-url? not-found-path
			     not-found-headers
			     (local-url port "servlets/some-program-name-that-is-not-there"))
	  (problem-with-url? servlet-error-path
			     servlet-error-headers
			     (local-url port "servlets/tests/broken.ss"))
	  (broken-url-regexp? (local-url port "servlets/tests/non-unit.ss")
			      non-unit-headers
			      "Servlet didn't load. \"Loading \\\".*non-unit.ss\\\" produced \n5\n instead of a servlet.\"")
          (broken-url-regexp? (local-url port "servlets/tests/bad-require.ss")
			      (append usual-headers (list generic-content-length-header))
			      "Servlet didn't load. open-input-file: cannot open input file: \".*not-there-on-purpose.ss\" (No such file or directory; errno=2)")
	  #|(problem-with-url? servlet-error-path
			       servlet-error-headers
			       (local-url port "servlets/tests/bad-return.ss"))
	  |#)))
  
  ; timeouts-broken? : nat -> (U false str)
  (define (timeouts-broken? port)
    (printf "timeouts-broken?:~n")
    (delimit-resources
     (lambda ()
       (let-values ([(in out) (tcp-connect TEST-IP port)])
         (sleep 40) ; must be big enough to timeout
         (if (and (char-ready? in) (eof-object? (read-char in)))
             #f
             "Did not timeout")))))
  
  ; local-url : nat str -> url
  (define (local-url port extra)
    (string->url (format "http://~a:~a/~a" TEST-IP port extra)))
  
  ; error-add : str (+ false str) -> (+ false str)
  (define (error-add prefix result)
    (and result (string-append prefix result)))
  
  ; mime-headers-problem? : (listof mime-header) header-pattern -> (+ false str)
  (define (mime-headers-problem? headers answers)
    (myprint "mime-headers-problem?~n")
    (if (not (= (length headers) (length answers)))
	(format "wrong number of headers.~n  expected ~s~n  received ~s"
		answers (map (lambda (h)
                               (list (mime-header-name h)
                                     (mime-header-value h)))
                             headers))
	(ormap (lambda (h a)
		 (if (and (string-ci=? (mime-header-name h)
                                       (symbol->string (car a)))
			  (regexp-match (cadr a) (mime-header-value h)))
		     #f
		     (format "mime-header results:~s" (list (list (car a) (mime-header-name h))
					(list (cadr a) (mime-header-value h))))))
	       headers
	       answers)))
  
  ; input-port-diff : iport iport -> (+ false str)
  ; effect: consumes all input on at least the shorter port
  (define (input-port-diff a b)
    (myprint "input-port-diff~n")
    (let compare ([n 0])
      (let* ([c-a (read-char a)]
	     [c-b (read-char b)]
	     [differ (lambda () (format "(<where> <actual> <expected>): ~s" (list n c-a c-b)))])
	(cond
	  [(eof-object? c-a)
	   (cond
	     [(eof-object? c-b) #f]
	     [else (differ)])]
	  [else
	   (cond
	     [(eof-object? c-b) (differ)]
	     [else (if (char=? c-a c-b)
		       (compare (add1 n))
		       (differ))])]))))
  
  ; extract-k-url : iport -> str
  ; to extract the action url from an html form
  (define (extract-k-url in)
    (or (let search-content ([el (document-element (read-xml in))])
	  (cond
	    [(element? el)
	     (or (if (eq? 'form (element-name el))
		     (let action-attribute ([attrs (element-attributes el)])
		       (cond
			 [(null? attrs) #f]
			 [else (if (eq? 'action (attribute-name (car attrs)))
				   (attribute-value (car attrs))
				   (action-attribute (cdr attrs)))]))
		     #f)
		 (ormap search-content (element-content el)))]
	    [else #f]))
	(raise "couldn't find action url")))
  
  #| 
  ; Meta tests:
  
  ; input-port-diff
  (call-with-input-file "/home/ptg/to-do"
    (lambda (in1)
      (call-with-input-file "/home/ptg/to-do"
	(lambda (in2)
	  (input-port-diff in1 in2)))))
  (call-with-input-file "/etc/passwd"
    (lambda (in1)
      (call-with-input-file "/home/ptg/to-do"
	(lambda (in2)
	  (input-port-diff in1 in2)))))
  |# #|
  (mime-headers-problem? (list (make-mime-header "server" ": PLT"))
			 '((server ": PLT")))
  (mime-headers-problem? (list (make-mime-header "server" ": PLT"))
			 '((server ": PLT2")))
  |#
  #|
  (string=? "boing" (extract-k-url (open-input-string "<html><form action='boing'>hi</form></html>")))
  (string=? "boing" (extract-k-url (open-input-string "<html><form><form action='boing'>hi</form></form></html>")))
  (with-handlers ([void (lambda (exn) (string=? exn "couldn't find action url"))])
    (extract-k-url (open-input-string "<html><p>pair of graph</p></html>"))
    #f)
  |#
  )
