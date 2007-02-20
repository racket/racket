
(module port mzscheme
  (require (lib "etc.ss")
	   (lib "contract.ss")
	   (lib "list.ss")
	   "private/port.ss")

  (define (exact-non-negative-integer? i)
    (and (number? i) (exact? i) (integer? i) (i . >= . 0)))

  (define (input-port-with-progress-evts? ip)
    (and (input-port? ip)
	 (port-provides-progress-evts? ip)))

  (define (mutable-bytes? b)
    (and (bytes? b) (not (immutable? b))))
  (define (mutable-string? b)
    (and (string? b) (not (immutable? b))))

  (define (line-mode-symbol? s)
    (memq s '(linefeed return return-linefeed any any-one)))

  (define (evt?/false v)
    (or (eq? #f v) (evt? v)))
  
  ;; ----------------------------------------

  (define (strip-shell-command-start in)
    (when (regexp-match-peek #rx#"^#![^\r\n]*" in)
      (let loop ([s (read-line in)])
	(when (regexp-match #rx#"\\\\$" s)
	  (loop (read-line in))))))

  ;; ----------------------------------------

  (define (copy-port src dest . dests)
    (unless (input-port? src)
      (raise-type-error 'copy-port "input-port" src))
    (for-each
     (lambda (dest)
       (unless (output-port? dest)
	 (raise-type-error 'copy-port "output-port" dest)))
     (cons dest dests))
    (let ([s (make-bytes 4096)]
	  [dests (cons dest dests)])
      (let loop ()
	(let ([c (read-bytes-avail! s src)])
	  (cond
	   [(number? c)
	    (for-each
	     (lambda (dest)
	       (let loop ([start 0])
		 (unless (= start c)
		   (let ([c2 (write-bytes-avail s dest start c)])
		     (loop (+ start c2))))))
	     dests)
	    (loop)]
	   [(procedure? c)
	    (let ([v (let-values ([(l col p) (port-next-location src)])
		       (c (object-name src) l col p))])
	      (for-each
	       (lambda (dest) (write-special v dest))
	       dests))
	    (loop)]
	   [else
	    ;; Must be EOF
	    (void)])))))
  
  (define merge-input
    (case-lambda
     [(a b) (merge-input a b 4096)]
     [(a b limit)
      (or (input-port? a)
	  (raise-type-error 'merge-input "input-port" a))
      (or (input-port? b)
	  (raise-type-error 'merge-input "input-port" b))
      (or (not limit)
	  (and (number? limit) (positive? limit) (exact? limit) (integer? limit))
	  (raise-type-error 'merge-input "positive exact integer or #f" limit))
      (let-values ([(rd wt) (make-pipe-with-specials limit)]
		   [(other-done?) #f]
		   [(sema) (make-semaphore 1)])
	(let ([copy
	       (lambda (from)
                 (thread 
                  (lambda ()
                    (copy-port from wt)
                    (semaphore-wait sema)
                    (if other-done?
                        (close-output-port wt)
                        (set! other-done? #t))
                    (semaphore-post sema))))])
	  (copy a)
	  (copy b)
	  rd))]))

  ;; `make-input-port/read-to-peek' sometimes need to wrap a special-value
  ;; procedure so that it's only called once when the value is both
  ;; peeked and read.
  (define-values (struct:memoized make-memoized memoized? memoized-ref memoized-set!)
    (make-struct-type 'memoized #f 1 0 #f null (current-inspector) 0))
  (define (memoize p)
    (define result #f)
    (make-memoized
     (if (procedure-arity-includes? p 0)
         ;; original p accepts 0 or 4 arguments:
         (case-lambda
          [() (unless result (set! result (box (p)))) (unbox result)]
          [(src line col pos)
           (unless result (set! result (box (p src line col pos))))
           (unbox result)])
         ;; original p accepts only 4 arguments:
         (lambda (src line col pos)
           (unless result (set! result (box (p src line col pos))))
           (unbox result)))))

  ;; Not kill-safe.
  ;; If the `read' proc returns an event, the event must produce 
  ;;  0 always
  (define make-input-port/read-to-peek 
    (opt-lambda (name read fast-peek close 
		      [location-proc #f] 
		      [count-lines!-proc void] 
		      [init-position 1]
		      [buffer-mode-proc #f])
      (define lock-semaphore (make-semaphore 1))
      (define commit-semaphore (make-semaphore 1))
      (define-values (peeked-r peeked-w) (make-pipe))
      (define special-peeked null)
      (define special-peeked-tail #f)
      (define progress-requested? #f)
      (define use-manager? #f)
      (define manager-th #f)
      (define manager-ch (make-channel))
      (define resume-ch (make-channel))
      (define buf (make-bytes 4096))
      (define (try-again)
	(wrap-evt
	 (semaphore-peek-evt lock-semaphore)
	 (lambda (x) 0)))
      (define (suspend-manager)
	(channel-put manager-ch 'suspend))
      (define (resume-manager)
	(channel-put resume-ch 'resume))
      (define (with-manager-lock thunk)
	(thread-resume manager-th (current-thread))
	(dynamic-wind suspend-manager thunk resume-manager))
      (define (make-progress)
	(write-byte 0 peeked-w)
	(read-byte peeked-r))
      (define (read-it-with-lock s)
	(if use-manager?
	    (with-manager-lock (lambda () (do-read-it s)))
	    (do-read-it s)))
      (define (read-it s)
	(call-with-semaphore
	 lock-semaphore
	 read-it-with-lock
	 try-again
	 s))
      (define (do-read-it s)
	(if (byte-ready? peeked-r)
            (read-bytes-avail!* s peeked-r)
	    ;; If nothing is saved from a peeking read,
	    ;; dispatch to `read', otherwise return
	    ;; previously peeked data
	    (cond
	     [(null? special-peeked) 
	      (when progress-requested? (make-progress))
	      (read s)]
	     [else (if (bytes? (car special-peeked))
		       (let ([b (car special-peeked)])
			 (write-bytes b peeked-w)
			 (set! special-peeked (cdr special-peeked))
			 (when (null? special-peeked)
			   (set! special-peeked-tail #f))
			 (read-bytes-avail!* s peeked-r))
		       (begin0
			(car special-peeked)
			(make-progress)
			(set! special-peeked (cdr special-peeked))
			(when (null? special-peeked)
			  (set! special-peeked-tail #f))))])))
      (define (peek-it-with-lock s skip unless-evt)
        (if use-manager?
            (with-manager-lock (lambda () (do-peek-it s skip unless-evt)))
            (do-peek-it s skip unless-evt)))
      (define (peek-it s skip unless-evt)
	(let ([v (peek-bytes-avail!* s skip unless-evt peeked-r)])
	  (if (eq? v 0)
	      (call-with-semaphore
	       lock-semaphore
	       peek-it-with-lock
	       try-again
	       s skip unless-evt)
	      v)))
      (define (do-peek-it s skip unless-evt)
	(let ([v (peek-bytes-avail!* s skip unless-evt peeked-r)])
	  (if (eq? v 0)
	      ;; The peek may have failed because peeked-r is empty,
	      ;; because unless-evt is ready, or because the skip is
	      ;; far. Handle nicely the common case where there are no
	      ;; specials.
	      (cond
	       [(and unless-evt (sync/timeout 0 unless-evt))
		#f]
	       [(null? special-peeked)
		;; Empty special queue, so read through the original proc.
		;; We only only need 
		;;  (- (+ skip (bytes-length s)) (pipe-content-length peeked-w))
		;; bytes, but read more (up to size of buf) to help move
		;; things along.
		(let* ([r (read buf)])
		  (cond
		   [(number? r)
		    ;; The nice case --- reading gave us more bytes
		    (write-bytes buf peeked-w 0 r)
		    ;; Now try again
		    (peek-bytes-avail!* s skip #f peeked-r)]
		   [(evt? r)
		    (if unless-evt
			;; Technically, there's a race condition here.
			;; We might choose r (and return 0) even when
			;; unless-evt becomes available first. However,
			;; this race is detectable only by the inside
			;; of `read'.
			(choice-evt r (wrap-evt unless-evt (lambda (x) #f)))
			r)]
		   [else
		    (set! special-peeked (cons r null))
		    (set! special-peeked-tail special-peeked)
		    ;; Now try again
		    (do-peek-it s skip unless-evt)]))]
	       [else
		;; Non-empty special queue, so try to use it
		(let* ([avail (pipe-content-length peeked-r)]
		       [sk (- skip avail)])
		  (let loop ([sk sk]
			     [l special-peeked])
		    (cond
		     [(null? l)
		      ;; Not enough even in the special queue.
		      ;; Read once and add it.
		      (let* ([t (make-bytes (min 4096 (+ sk (bytes-length s))))]
			     [r (read t)])
			(cond
			 [(evt? r) 
			  (if unless-evt
			      ;; See note above
			      (choice-evt r (wrap-evt unless-evt (lambda (x) #f)))
			      r)]
			 [(eq? r 0) 
			  ;; Original read thinks a spin is ok,
			  ;;  so we return 0 to skin, too.
			  0]
			 [else (let ([v (if (number? r)
					    (subbytes t 0 r)
					    r)])
				 (let ([pr (cons v null)])
				   (set-cdr! special-peeked-tail pr)
				   (set! special-peeked-tail pr))
				 ;; Got something; now try again
				 (do-peek-it s skip unless-evt))]))]
		     [(eof-object? (car l)) 
		      ;; No peeking past an EOF
		      eof]
		     [(procedure? (car l))
		      (if (zero? sk)
                          ;; We should call the procedure only once. Change
                          ;;  (car l) to a memoizing function, if it isn't already:
                          (let ([proc (car l)])
                            (if (memoized? proc)
                                proc
                                (let ([proc (memoize proc)])
                                  (set-car! l proc)
                                  proc)))
                          ;; Skipping over special...
			  (loop (sub1 sk) (cdr l)))]
		     [(bytes? (car l))
		      (let ([len (bytes-length (car l))])
			(if (sk . < . len)
			    (let ([n (min (bytes-length s)
					  (- len sk))])
			      (bytes-copy! s 0 (car l) sk (+ sk n))
			      n)
			    (loop (- sk len) (cdr l))))])))])
	      v)))
      (define (commit-it-with-lock amt unless-evt done-evt)
	(if use-manager?
	    (with-manager-lock (lambda () (do-commit-it amt unless-evt done-evt)))
	    (do-commit-it amt unless-evt done-evt)))
      (define (commit-it amt unless-evt done-evt)
	(call-with-semaphore
	 lock-semaphore
	 commit-it-with-lock
	 #f
	 amt unless-evt done-evt))
      (define (do-commit-it amt unless-evt done-evt)
	(if (sync/timeout 0 unless-evt)
	    #f
	    (let* ([avail (pipe-content-length peeked-r)]
		   [p-commit (min avail amt)])
	      (let loop ([amt (- amt p-commit)]
			 [l special-peeked])
		(cond
		 [(amt . <= . 0)
		  ;; Enough has been peeked. Do commit...
		  (actual-commit p-commit l unless-evt done-evt)]
		 [(null? l)
		  ;; Requested commit was larger than previous peeks
		  #f]
		 [(bytes? (car l))
		  (let ([bl (bytes-length (car l))])
		    (if (bl . > . amt)
			;; Split the string
			(let ([next (cons
				     (subbytes (car l) amt)
				     (cdr l))])
			  (set-car! l (subbytes (car l) 0 amt))
			  (set-cdr! l next)
			  (when (eq? l special-peeked-tail)
			    (set! special-peeked-tail next))
			  (loop 0 (cdr l)))
			;; Consume this string...
			(loop  (- amt bl) (cdr l))))]
		 [else
		  (loop (sub1 amt) (cdr l))])))))
      (define (actual-commit p-commit l unless-evt done-evt)
	;; The `finish' proc finally, actually, will commit...
	(define (finish)
	  (unless (zero? p-commit)
	    (peek-byte peeked-r (sub1 p-commit))
	    (port-commit-peeked p-commit unless-evt always-evt peeked-r))
	  (set! special-peeked l)
	  (when (null? special-peeked)
	    (set! special-peeked-tail #f))
	  (when (and progress-requested? (zero? p-commit))
	    (make-progress))
	  #t)
	;; If we can sync done-evt immediately, then finish.
	(if (sync/timeout 0 (wrap-evt done-evt (lambda (x) #t)))
	    (finish)
	    ;; We need to wait, so we'll have to release the lock.
	    ;; Send the work to a manager thread.
	    (let ([result-ch (make-channel)]
		  [w/manager? use-manager?])
	      (if w/manager?
		  ;; Resume manager if it was running:
		  (resume-manager)
		  ;; Start manager if it wasn't running:
		  (begin
		    (set! manager-th (thread manage-commits))
		    (set! use-manager? #t)
		    (thread-resume manager-th (current-thread))))
	      ;; Sets use-manager? if the manager wasn't already running:
	      (channel-put manager-ch (list finish unless-evt done-evt result-ch))
	      ;; Release locks:
	      (semaphore-post lock-semaphore)
	      (begin0
	       ;; Wait for manager to complete commit:
	       (sync result-ch)
	       ;; Grab locks again, so they're released
	       ;;  properly on exit:
	       (semaphore-wait lock-semaphore)
	       (when w/manager?
		 (suspend-manager))))))
      (define (manage-commits)
	(let loop ([commits null])
	  (apply
	   sync
	   (handle-evt manager-ch
		       (lambda (c)
			 (case c
			   [(suspend)
			    (channel-get resume-ch)
			    (loop commits)]
			   [else
			    ;; adding a commit
			    (loop (cons c commits))])))
	   (map (lambda (c)
		  (define (send-result v)
		    ;; Create a new thread to send the result asynchronously:
		    (thread-resume
		     (thread (lambda ()
			       (channel-put (list-ref c 3) v)))
		     (current-thread))
		    (when (null? (cdr commits))
		      (set! use-manager? #f))
		    (loop (remq c commits)))
		  ;; Choose between done and unless:
		  (if (sync/timeout 0 (list-ref c 1))
		      (handle-evt always-evt
				  (lambda (x)
				    (send-result #f)))
		      (choice-evt
		       (handle-evt (list-ref c 1)
				   (lambda (x)
				     ;; unless ready, which means that the commit must fail
				     (send-result #f)))
		       (handle-evt (list-ref c 2)
				   (lambda (x)
				     ;; done-evt ready, which means that the commit
				     ;;  must succeed.
				     ;; If we get here, then commits are not
				     ;; suspended, so we implicitly have the
				     ;; lock.
				     ((list-ref c 0))
				     (send-result #t))))))
		commits))))
      (make-input-port
       name
       ;; Read
       read-it
       ;; Peek
       (if fast-peek
	   (let ([fast-peek-k (lambda (s skip)
				(peek-it s skip #f))])
	     (lambda (s skip unless-evt)
	       (if (or unless-evt
		       (byte-ready? peeked-r)
		       (pair? special-peeked))
		   (peek-it s skip unless-evt)
		   (fast-peek s skip fast-peek-k))))
	   peek-it)
       close
       (lambda ()
	 (set! progress-requested? #t)
	 (port-progress-evt peeked-r))
       commit-it
       location-proc
       count-lines!-proc
       init-position
       buffer-mode-proc)))

  (define peeking-input-port
    (opt-lambda (orig-in [name (object-name orig-in)] [delta 0])
      (make-input-port/read-to-peek
       name
       (lambda (s)
	 (let ([r (peek-bytes-avail!* s delta #f orig-in)])
	   (set! delta (+ delta (cond
				 [(number? r) r]
				 [else 1])))
	   (if (eq? r 0)
	       (handle-evt orig-in (lambda (v) 0))
	       r)))
       (lambda (s skip default)
	 (peek-bytes-avail!* s (+ delta skip) #f orig-in))
       void)))

  (define relocate-input-port
    (opt-lambda (p line col pos [close? #t])
      (transplant-to-relocate
       transplant-input-port
       p line col pos close?)))

  (define transplant-input-port
    (opt-lambda (p location-proc pos [close? #t] [count-lines!-proc void])
      (make-input-port
       (object-name p)
       (lambda (s) (let ([v (read-bytes-avail!* s p)])
		     (if (eq? v 0)
			 (wrap-evt p (lambda (x) 0))
			 v)))
       (lambda (s skip evt) 
	 (let ([v (peek-bytes-avail!* s skip evt p)])
	   (if (eq? v 0)
	       (choice-evt
		(wrap-evt p (lambda (x) 0))
		(if evt
		    (wrap-evt evt (lambda (x) #f))
		    never-evt))
	       v)))
       (lambda ()
	 (when close?
	   (close-input-port p)))
       (and (port-provides-progress-evts? p)
	    (lambda ()
	      (port-progress-evt p)))
       (and (port-provides-progress-evts? p)
	    (lambda (n evt target-evt)
	      (port-commit-peeked n evt target-evt p)))
       location-proc
       count-lines!-proc
       pos)))

  ;; Not kill-safe.
  (define make-pipe-with-specials
    ;; This implementation of pipes is almost CML-style, with a manager thread
    ;; to guard access to the pipe content. But we only enable the manager
    ;; thread when write evts are active; otherwise, we use a lock semaphore.
    ;; (Actually, the lock semaphore has to be used all the time, to guard
    ;; the flag indicating whether the manager thread is running.)
    (opt-lambda ([limit (expt 2 64)] [in-name 'pipe] [out-name 'pipe])
      (let-values ([(r w) (make-pipe limit)]
		   [(more) null]
		   [(more-last) #f]
		   [(more-sema) #f]
		   [(close-w?) #f]
		   [(lock-semaphore) (make-semaphore 1)]
		   [(mgr-th) #f]
		   [(via-manager?) #f]
		   [(mgr-ch) (make-channel)])
	(define (flush-more)
	  (if (null? more)
	      (begin
		(set! more-last #f)
		(when close-w?
		  (close-output-port w)))
	      (when (bytes? (car more))
		(let ([amt (bytes-length (car more))])
		  (let ([wrote (write-bytes-avail* (car more) w)])
		    (if (= wrote amt)
			(begin
			  (set! more (cdr more))
			  (flush-more))
			(begin
			  ;; This means that we let too many bytes
			  ;;  get written while a special was pending.
			  ;;  (The limit is disabled when a special
                          ;;  is in the pipe.)
			  (set-car! more (subbytes (car more) wrote))
			  ;; By peeking, make room for more:
			  (peek-byte r (sub1 (min (pipe-content-length w)
						  (- amt wrote))))
			  (flush-more))))))))
	(define (read-one s)
	  (let ([v (read-bytes-avail!* s r)])
	    (if (eq? v 0)
		(if more-last
		    ;; Return a special
		    (let ([a (car more)])
		      (set! more (cdr more))
		      (flush-more)
		      (lambda (file line col ppos)
			a))
		    ;; Nothing available, yet.
		    (begin
		      (unless more-sema
			(set! more-sema (make-semaphore)))
		      (wrap-evt (semaphore-peek-evt more-sema)
				(lambda (x) 0))))
		v)))
	(define (close-it)
	  (set! close-w? #t)
	  (unless more-last
	    (close-output-port w))
	  (when more-sema
	    (semaphore-post more-sema)))
	(define (write-these-bytes str start end)
	  (begin0
	   (if more-last
	       (let ([p (cons (subbytes str start end) null)])
		 (set-cdr! more-last p)
		 (set! more-last p)
		 (- end start))
	       (let ([v (write-bytes-avail* str w start end)])
		 (if (zero? v)
		     (wrap-evt w (lambda (x) #f))
		     v)))
	   (when more-sema
	     (semaphore-post more-sema)
	     (set! more-sema #f))))
	(define (write-spec v)
	  (let ([p (cons v null)])
	    (if more-last
		(set-cdr! more-last p)
		(set! more p))
	    (set! more-last p)
	    (when more-sema
	      (semaphore-post more-sema)
	      (set! more-sema #f))))
	(define (serve)
	  ;; A request is
	  ;;  (list sym result-ch nack-evt . v)
	  ;; where `v' varies for different `sym's
	  ;; The possible syms are: read, reply, close, 
	  ;;  write, write-spec, write-evt, write-spec-evt
	  (let loop ([reqs null])
	    (apply
	     sync
	     ;; Listen for a request:
	     (handle-evt mgr-ch
			 (lambda (req)
			   (let ([req
				  ;; Most requests we handle immediately and
				  ;; convert to a reply. The manager thread
				  ;; implicitly has the lock.
				  (let ([reply (lambda (v)
						 (list 'reply (cadr req) (caddr req) v))])
				    (case (car req)
				      [(read)
				       (reply (read-one (cadddr req)))]
				      [(close)
				       (reply (close-it))]
				      [(write)
				       (reply (apply write-these-bytes (cdddr req)))]
				      [(write-spec)
				       (reply (write-spec (cadddr req)))]
				      [else req]))])
			     (loop (cons req reqs)))))
	     (if (and (null? reqs)
		      via-manager?)
		 ;; If we can get the lock before another request
		 ;;  turn off manager mode:
		 (handle-evt lock-semaphore
			     (lambda (x)
			       (set! via-manager? #f)
			       (semaphore-post lock-semaphore)
			       (loop null)))
		 never-evt)
	     (append
	      (map (lambda (req)
		     (case (car req)
		       [(reply) (handle-evt (channel-put-evt (cadr req)
							     (cadddr req))
					    (lambda (x)
					      (loop (remq req reqs))))]
		       [(write-spec-evt) (if close-w?
					     ;; Report close error:
					     (handle-evt (channel-put-evt (cadr req) 'closed)
						    (lambda (x)
						      (loop (remq req reqs))))
					     ;; Try to write special:
					     (handle-evt (channel-put-evt (cadr req) #t)
							 (lambda (x)
							   ;; We sync'd, so now we *must* write
							   (write-spec (cadddr req))
							   (loop (remq req reqs)))))]
		       [(write-evt) (if close-w?
					;; Report close error:
					(handle-evt (channel-put-evt (cadr req) 'closed)
						    (lambda (x)
						      (loop (remq req reqs))))
					;; Try to write bytes:
					(let* ([start (list-ref req 4)]
					       [end (list-ref req 5)]
					       [len (if more-last
							(- end start)
							(min (- end start)
							     (max 0
								  (- limit (pipe-content-length w)))))])
					  (if (and (zero? len)
                                                   (null? more))
					      (handle-evt w (lambda (x) (loop reqs)))
					      (handle-evt (channel-put-evt (cadr req) len)
							  (lambda (x)
							    ;; We sync'd, so now we *must* write
							    (write-these-bytes (cadddr req) start (+ start len))
							    (loop (remq req reqs)))))))]))
		   reqs)
	      ;; nack => remove request (could be anything)
	      (map (lambda (req)
		     (handle-evt (caddr req)
				 (lambda (x)
				   (loop (remq req reqs)))))
		   reqs)))))
	(define (via-manager what req-sfx)
	  (thread-resume mgr-th (current-thread))
	  (let ([ch (make-channel)])
	    (sync (nack-guard-evt
		   (lambda (nack)
		     (channel-put mgr-ch (list* what ch nack req-sfx))
		     ch)))))
	(define (start-mgr)
	  (unless mgr-th
	    (set! mgr-th (thread serve)))
	  (set! via-manager? #t))
	(define (evt what req-sfx)
	  (nack-guard-evt
	   (lambda (nack)
	     (resume-mgr)
	     (let ([ch (make-channel)])
	       (call-with-semaphore
		lock-semaphore
		(lambda ()
		  (unless mgr-th
		    (set! mgr-th (thread serve)))
		  (set! via-manager? #t)
		  (thread-resume mgr-th (current-thread))
		  (channel-put mgr-ch (list* what ch nack req-sfx))
		  (wrap-evt ch (lambda (x)
				 (if (eq? x 'close)
				     (raise-mismatch-error 'write-evt "port is closed: " out)
				     x)))))))))
	(define (resume-mgr)
	  (when mgr-th
	    (thread-resume mgr-th (current-thread))))
	(define in 
	  ;; ----- Input ------
	  (make-input-port/read-to-peek 
	   in-name
	   (lambda (s)
	     (let ([v (read-bytes-avail!* s r)])
	       (if (eq? v 0)
		   (begin
		     (resume-mgr)
		     (call-with-semaphore
		      lock-semaphore
		      (lambda ()
			(if via-manager?
			    (via-manager 'read (list s))
			    (read-one s)))))
		   v)))
	   #f
	   void))
	(define out
	  ;; ----- Output ------
	  (make-output-port
	   out-name
	   w
	   ;; write
	   (lambda (str start end buffer? w/break?)
	     (if (= start end)
		 #t
		 (begin
		   (resume-mgr)
		   (call-with-semaphore
		    lock-semaphore
		    (lambda ()
		      (if via-manager?
			  (via-manager 'write (list str start end))
			  (write-these-bytes str start end)))))))
	   ;; close
	   (lambda ()
	     (resume-mgr)
	     (call-with-semaphore
	      lock-semaphore
	      (lambda ()
		(if via-manager?
		    (via-manager 'close null)
		    (close-it)))))
	   ;; write-special
	   (lambda (v buffer? w/break?)
	     (resume-mgr)
	     (call-with-semaphore
	      lock-semaphore
	      (lambda ()
		(if via-manager?
		    (via-manager 'write-spec (list v))
		    (write-spec v)))))
	   ;; write-evt
	   (lambda (str start end)
	     (if (= start end)
		 (wrap-evt always-evt (lambda (x) 0))
		 (evt 'write-evt (list str start end))))
	   ;; write-special-evt
	   (lambda (v)
	     (evt 'write-spec-evt (list v)))))
	(values in out))))
		   

  (define input-port-append
    (opt-lambda (close-orig? . ports)
      (make-input-port
       (map object-name ports)
       (lambda (str)
	 ;; Reading is easy -- read from the first port,
	 ;;  and get rid of it if the result is eof
	 (if (null? ports)
	     eof
	     (let ([n (read-bytes-avail!* str (car ports))])
	       (cond
		[(eq? n 0) (wrap-evt (car ports) (lambda (x) 0))]
		[(eof-object? n)
		 (when close-orig?
		   (close-input-port (car ports)))
		 (set! ports (cdr ports))
		 0]
		[else n]))))
       (lambda (str skip unless-evt)
	 ;; Peeking is more difficult, due to skips.
	 (let loop ([ports ports][skip skip])
	   (if (null? ports)
	       eof
	       (let ([n (peek-bytes-avail!* str skip unless-evt (car ports))])
		 (cond
		  [(eq? n 0) 
		   ;; Not ready, yet.
		   (peek-bytes-avail!-evt str skip unless-evt (car ports))]
		  [(eof-object? n)
		   ;; Port is exhausted, or we skipped past its input.
		   ;; If skip is not zero, we need to figure out
		   ;;  how many chars were skipped.
		   (loop (cdr ports)
			 (- skip (compute-avail-to-skip skip (car ports))))]
		  [else n])))))
       (lambda ()
	 (when close-orig?
	   (map close-input-port ports))))))

  (define (convert-stream from from-port
			  to to-port)
    (let ([c (bytes-open-converter from to)]
	  [in (make-bytes 4096)]
	  [out (make-bytes 4096)])
      (unless c
	(error 'convert-stream "could not create converter from ~e to ~e"
	       from to))
      (dynamic-wind
	  void
	  (lambda ()
	    (let loop ([got 0])
	      (let ([n (read-bytes-avail! in from-port got)])
		(let ([got (+ got (if (number? n)
				      n
				      0))])
		  (let-values ([(wrote used status) (bytes-convert c in 0 got out)])
		    (when (eq? status 'error)
		      (error 'convert-stream "conversion error"))
		    (unless (zero? wrote)
		      (write-bytes out to-port 0 wrote))
		    (bytes-copy! in 0 in used got)
		    (if (not (number? n))
			(begin
			  (unless (= got used)
			    (error 'convert-stream "input stream ~a with a partial conversion"
				   (if (eof-object? n) "ended" "hit a special value")))
			  (let-values ([(wrote status) (bytes-convert-end c out)])
			    (when (eq? status 'error)
			      (error 'convert-stream "conversion-end error"))
			    (unless (zero? wrote)
			      (write-bytes out to-port 0 wrote))
			    (if (eof-object? n)
				;; Success
				(void)
				(begin
				  (write-special n to-port)
				  (loop 0)))))
			(loop (- got used))))))))
	  (lambda () (bytes-close-converter c)))))

  ;; Helper for input-port-append; given a skip count
  ;;  and an input port, determine how many characters
  ;;  (up to upto) are left in the port. We figure this
  ;;  out using binary search.
  (define (compute-avail-to-skip upto p)
    (let ([str (make-bytes 1)])
      (let loop ([upto upto][skip 0])
	(if (zero? upto)
	    skip
	    (let* ([half (quotient upto 2)]
		   [n (peek-bytes-avail!* str (+ skip half) #f p)])
	      (if (eq? n 1)
		  (loop (- upto half 1) (+ skip half 1))
		  (loop half skip)))))))

  (define make-limited-input-port
    (opt-lambda (port limit [close-orig? #t])
      (let ([got 0])
	(make-input-port
	 (object-name port)
	 (lambda (str)
	   (let ([count (min (- limit got) (bytes-length str))])
	     (if (zero? count)
		 eof
		 (let ([n (read-bytes-avail!* str port 0 count)])
		   (cond
		    [(eq? n 0) (wrap-evt port (lambda (x) 0))]
		    [(number? n) (set! got (+ got n)) n]
		    [(procedure? n) (set! got (add1 got)) n]
		    [else n])))))
	 (lambda (str skip progress-evt)
	   (let ([count (max 0 (min (- limit got skip) (bytes-length str)))])
	     (if (zero? count)
		 eof
		 (let ([n (peek-bytes-avail!* str skip progress-evt port 0 count)])
		   (if (eq? n 0)
		       (wrap-evt port (lambda (x) 0))
		       n)))))
	 (lambda ()
	   (when close-orig?
	     (close-input-port port)))))))
	      
  ;; ----------------------------------------

  (define (poll-or-spawn go)
    (poll-guard-evt
     (lambda (poll?)
       (if poll?
	   ;; In poll mode, call `go' directly:
	   (let ([v (go never-evt #f #t)])
	     (if v
		 (wrap-evt always-evt (lambda (x) v))
		 never-evt))
	   ;; In non-poll mode, start a thread to call go
	   (nack-guard-evt
	    (lambda (nack)
	      (define ch (make-channel))
	      (define ready (make-semaphore))
	      (let ([t (thread (lambda () 
				 (parameterize-break #t
				   (with-handlers ([exn:break? void])
				     (semaphore-post ready)
				     (go nack ch #f)))))])
		(thread (lambda ()
			  (sync nack) 
			  (semaphore-wait ready)
			  (break-thread t))))
	      ch))))))

  (define (read-at-least-bytes!-evt orig-bstr input-port need-more? shrink combo 
				    peek-offset prog-evt)
    ;; go is the main reading function, either called directly for
    ;; a poll, or called in a thread for a non-poll read
    (define (go nack ch poll?)
      (let try-again ([pos 0][bstr orig-bstr])
	(let* ([progress-evt (or prog-evt 
				 (port-progress-evt input-port))]
	       [v ((if poll? 
		       peek-bytes-avail!* 
		       peek-bytes-avail!)
		   bstr (+ pos (or peek-offset 0)) progress-evt input-port pos)])
	  (cond
	   ;; the first two cases below are shortcuts, and not
	   ;;  strictly necessary
	   [(sync/timeout 0 nack) (void)]
	   [(sync/timeout 0 progress-evt) (if poll?
					      #f
					      (if prog-evt
						  (void)
						  (try-again pos bstr)))]
	   [(and poll? (equal? v 0)) #f]
	   [(and (number? v) (need-more? bstr (+ pos v)))
	    => (lambda (bstr)
		 (try-again (+ v pos) bstr))]
	   [else
	    (let* ([v2 (cond
			[(number? v) (shrink bstr (+ v pos))]
			[(positive? pos) pos]
			[else v])]
		   [result (combo bstr v2)])
	      (cond
	       [peek-offset
		(if poll?
		    result
		    (sync (or prog-evt never-evt)
			  (channel-put-evt ch result)))]
	       [(port-commit-peeked (if (number? v2) v2 1)
				    progress-evt
				    (if poll?
					always-evt
					(channel-put-evt ch result))
				    input-port)
		result]
	       [(and (eof-object? eof)
		     (zero? pos)
		     (not (sync/timeout 0 progress-evt)))
		;; Must be a true end-of-file
		(let ([result (combo bstr eof)])
		  (if poll?
		      result
		      (channel-put ch result)))]
	       [poll? #f]
	       [else (try-again 0 orig-bstr)]))]))))
    (if (zero? (bytes-length orig-bstr))
	(wrap-evt always-evt (lambda (x) 0))
	(poll-or-spawn go)))
  
  (define (-read-bytes-avail!-evt bstr input-port peek-offset prog-evt)
    (read-at-least-bytes!-evt bstr input-port 
			      (lambda (bstr v) (if (zero? v)
						   bstr
						   #f))
			      (lambda (bstr v) v)
			      (lambda (bstr v) v)
			      peek-offset prog-evt))

  (define (read-bytes-avail!-evt bstr input-port)
    (-read-bytes-avail!-evt bstr input-port #f #f))

  (define (peek-bytes-avail!-evt bstr peek-offset prog-evt input-port)
    (-read-bytes-avail!-evt bstr input-port peek-offset prog-evt))

  (define (-read-bytes!-evt bstr input-port peek-offset prog-evt)
    (read-at-least-bytes!-evt bstr input-port 
			      (lambda (bstr v)
				(if (v . < . (bytes-length bstr))
				    bstr
				    #f))
			      (lambda (bstr v) v)
			      (lambda (bstr v) v)
			      peek-offset prog-evt))

  (define (read-bytes!-evt bstr input-port)
    (-read-bytes!-evt bstr input-port #f #f))
  
  (define (peek-bytes!-evt bstr peek-offset prog-evt input-port)
    (-read-bytes!-evt bstr input-port peek-offset prog-evt))
  
  (define (-read-bytes-evt len input-port peek-offset prog-evt)
    (let ([bstr (make-bytes len)])
      (wrap-evt
       (-read-bytes!-evt bstr input-port peek-offset prog-evt)
       (lambda (v)
	 (if (number? v)
	     (if (= v len)
		 bstr
		 (subbytes bstr 0 v))
	     v)))))

  (define (read-bytes-evt len input-port)
    (-read-bytes-evt len input-port #f #f))
	   
  (define (peek-bytes-evt len peek-offset prog-evt input-port)
    (-read-bytes-evt len input-port peek-offset prog-evt))
  
  (define (-read-string-evt goal input-port peek-offset prog-evt)
    (if (zero? goal)
	(wrap-evt always-evt (lambda (x) ""))
	(let ([bstr (make-bytes goal)]
	      [c (bytes-open-converter "UTF-8-permissive" "UTF-8")])
	  (wrap-evt
	   (read-at-least-bytes!-evt bstr input-port 
				     (lambda (bstr v)
				       (if (= v (bytes-length bstr))
					   ;; We can't easily use bytes-utf-8-length here,
					   ;; because we may need more bytes to figure out
					   ;; the true role of the last byte. The
					   ;; `bytes-convert' function lets us deal with
					   ;; the last byte properly.
					   (let-values ([(bstr2 used status) 
							 (bytes-convert c bstr 0 v)])
					     (let ([got (bytes-utf-8-length bstr2)])
					       (if (= got goal)
						   ;; Done:
						   #f 
						   ;; Need more bytes:
						   (let ([bstr2 (make-bytes (+ v (- goal got)))])
						     (bytes-copy! bstr2 0 bstr)
						     bstr2))))
					   ;; Need more bytes in bstr:
					   bstr))
				     (lambda (bstr v)
				       ;; We may need one less than v,
				       ;; because we may have had to peek
				       ;; an extra byte to discover an
				       ;; error in the stream.
				       (if ((bytes-utf-8-length bstr #\? 0 v) . > . goal)
					   (sub1 v)
					   v))
				     cons
				     peek-offset prog-evt)
	   (lambda (bstr+v)
	     (let ([bstr (car bstr+v)]
		   [v (cdr bstr+v)])
	       (if (number? v)
		   (bytes->string/utf-8 bstr #\? 0 v)
		   v)))))))

  (define (read-string-evt goal input-port)
    (-read-string-evt goal input-port #f #f))
    
  (define (peek-string-evt goal peek-offset prog-evt input-port)
    (-read-string-evt goal input-port peek-offset prog-evt))

  (define (-read-string!-evt str input-port peek-offset prog-evt)
    (wrap-evt
     (-read-string-evt (string-length str) input-port peek-offset prog-evt)
     (lambda (s)
       (if (string? s)
	   (begin
	     (string-copy! str 0 s)
	     (string-length s))
	   s))))

  (define (read-string!-evt str input-port)
    (-read-string!-evt str input-port #f #f))

  (define (peek-string!-evt str peek-offset prog-evt input-port)
    (-read-string!-evt str input-port peek-offset prog-evt))

  (define (regexp-match-evt pattern input-port)
    (define (go nack ch poll?)
      (let try-again ()
	(let* ([progress-evt (port-progress-evt input-port)]
	       [m ((if poll?
		       regexp-match-peek-positions-immediate 
		       regexp-match-peek-positions)
		   pattern input-port 0 #f progress-evt)])
	  (cond
	   [(sync/timeout 0 nack) (void)]
	   [(sync/timeout 0 progress-evt) (try-again)]
	   [(not m)
	    (if poll?
		#f
		(sync nack
		      (handle-evt progress-evt
				  (lambda (x) (try-again)))))]
	   [else
	    (let ([m2 (map (lambda (p)
			     (and p
				  (let ([bstr (make-bytes (- (cdr p) (car p)))])
				    (unless (= (car p) (cdr p))
				      (let loop ([offset 0])
					(let ([v (peek-bytes-avail! bstr (car p) progress-evt input-port offset)])
					  (unless (zero? v)
					    (when ((+ offset v) . < . (bytes-length bstr))
					      (loop (+ offset v)))))))
				    bstr)))
			   m)])
	      (cond
	       [(and (zero? (cdar m))
		     (or poll?
			 (channel-put ch m2)))
		m2]
	       [(port-commit-peeked (cdar m) 
				    progress-evt 
				    (if poll?
					always-evt
					(channel-put-evt ch m2))
				    input-port)
		m2]
	       [poll? #f]
	       [else (try-again)]))]))))
    (poll-or-spawn go))

  (define-syntax (newline-rx stx)
    (syntax-case stx ()
      [(_ str) (datum->syntax-object #'here
				     (byte-regexp 
				      (string->bytes/latin-1
				       (format "^(?:(.*?)~a)|(.*?$)"
					       (syntax-e #'str)))))]))

  (define read-bytes-line-evt
    (opt-lambda (input-port [mode 'linefeed])
      (wrap-evt
       (regexp-match-evt (case mode
			   [(linefeed) (newline-rx "\n")]
			   [(return) (newline-rx "\r")]
			   [(return-linefeed) (newline-rx "\r\n")]
			   [(any) (newline-rx "(?:\r\n|\r|\n)")]
			   [(any-one) (newline-rx "[\r\n]")])
			 input-port)
       (lambda (m)
	 (or (cadr m)
	     (let ([l (caddr m)])
	       (if (and l (zero? (bytes-length l)))
		   eof
		   l)))))))
  
  (define read-line-evt
    (opt-lambda (input-port [mode 'linefeed])
      (wrap-evt
       (read-bytes-line-evt input-port mode)
       (lambda (s)
	 (if (eof-object? s)
	     s
	     (bytes->string/utf-8 s #\?))))))

  (define (eof-evt input-port)
    (wrap-evt
     (regexp-match-evt #rx#"^$" input-port)
     (lambda (x)
       eof)))

  ;; --------------------------------------------------
  
  (define reencode-input-port
    (opt-lambda (port encoding [error-bytes #f] [close? #f] [name (object-name port)])
      (let ([c (bytes-open-converter encoding "UTF-8")]
	    [ready-bytes (make-bytes 1024)]
	    [ready-start 0]
	    [ready-end 0]
	    [buf (make-bytes 1024)]
	    [buf-start 0]
	    [buf-end 0]
	    [buf-eof? #f]
	    [buf-eof-result #f]
	    [buffer-mode (or (file-stream-buffer-mode port)
			     'none)])
	;; Main reader entry:
	(define (read-it s)
	  (cond
	   [(> ready-end ready-start)
	    ;; We have leftover converted bytes:
	    (let ([cnt (min (bytes-length s)
			    (- ready-end ready-start))])
	      (bytes-copy! s 0 ready-bytes ready-start (+ ready-start cnt))
	      (set! ready-start (+ ready-start cnt))
	      cnt)]
	   [else
	    ;; Try converting already-read bytes:
	    (let-values ([(got-c used-c status) (if (= buf-start buf-end)
						    (values 0 0 'aborts)
						    (bytes-convert c buf buf-start buf-end s))])
	      (cond
	       [(positive? got-c)
		;; We converted some bytes into s.
		(set! buf-start (+ used-c buf-start))
		got-c]
	       [(eq? status 'aborts)
		(if buf-eof?
		    ;; Had an EOF or special in the stream.
		    (if (= buf-start buf-end)
			(begin0
			 buf-eof-result
			 (set! buf-eof? #f)
			 (set! buf-eof-result #f))
			(handle-error s))
		    ;; Need more bytes.
		    (begin
		      (when (positive? buf-start)
			(bytes-copy! buf 0 buf buf-start buf-end)
			(set! buf-end (- buf-end buf-start))
			(set! buf-start 0))
		      (let* ([amt (bytes-length s)]
			     [c (read-bytes-avail!* buf port buf-end
						    (if (eq? buffer-mode 'block)
							(bytes-length buf)
							(min (bytes-length buf) 
							     (+ buf-end amt))))])
			(cond
			 [(or (eof-object? c) 
			      (procedure? c))
			  ;; Got EOF/procedure
			  (set! buf-eof? #t)
			  (set! buf-eof-result c)
			  (read-it s)]
			 [(zero? c)
			  ;; No bytes ready --- try again later.
			  (wrap-evt port (lambda (v) 0))]
			 [else
			  ;; Got some bytes; loop to decode.
			  (set! buf-end (+ buf-end c))
			  (read-it s)]))))]
	       [(eq? status 'error)
		(handle-error s)]
	       [(eq? status 'continues)
		;; Need more room to make progress at all.
		;; Decode into ready-bytes.
		(let-values ([(got-c used-c status) (bytes-convert c buf buf-start buf-end ready-bytes)])
		  (unless (memq status '(continues complete))
		    (error 'reencode-input-port-read
			   "unable to make decoding progress: ~e"
			   port))
		  (set! ready-start 0)
		  (set! ready-end got-c)
		  (set! buf-start (+ used-c buf-start))
		  (read-it s))]))]))

	;; Raise exception or discard first buffered byte.
	;;  We assume that read-bytes is empty
	(define (handle-error s)
	  (if error-bytes
	      (begin
		(set! buf-start (add1 buf-start))
		(let ([cnt (min (bytes-length s)
				(bytes-length error-bytes))])
		  (bytes-copy! s 0 error-bytes 0 cnt)
		  (bytes-copy! ready-bytes 0 error-bytes cnt)
		  (set! ready-start 0)
		  (set! ready-end (- (bytes-length error-bytes) cnt))
		  cnt))
	      (error
	       'converting-input-port
	       "decoding error in input stream: ~e"
	       port)))

	(unless c
	  (error 'reencode-input-port 
		 "could not create converter from ~e to UTF-8"
		 encoding))
	
	(make-input-port/read-to-peek
	 name 
	 read-it
	 #f
	 (lambda ()
	   (when close?
	     (close-input-port port))
	   (bytes-close-converter c))
	 #f void 1
	 (case-lambda 
	  [() buffer-mode]
	  [(mode) (set! buffer-mode mode)])))))
  
  ;; --------------------------------------------------
  
  (define reencode-output-port
    (opt-lambda (port encoding [error-bytes #f] [close? #f] [name (object-name port)])
      (let ([c (bytes-open-converter "UTF-8" encoding)]
	    [ready-bytes (make-bytes 1024)]
	    [ready-start 0]
	    [ready-end 0]
	    [out-bytes (make-bytes 1024)]
	    [out-start 0]
	    [out-end 0]
	    [buffer-mode (or (file-stream-buffer-mode port)
			     'block)])
	
	;; The main writing entry point:
	(define (write-it s start end no-buffer&block? enable-break?)
	  (cond
	   [(= start end)
	    ;; This is a flush request; no-buffer&block? must be #f
	    ;; Note: we could get stuck because only half an encoding
	    ;;  is available in out-bytes.
	    (flush-some #f enable-break?)
	    (if (buffer-flushed?)
		0
		(write-it s start end no-buffer&block? enable-break?))]
	   [no-buffer&block?
	    (case (flush-all #t enable-break?)
	      [(not-done)
	       ;; We couldn't flush right away, so give up.
	       #f]
	      [(done)
	       (non-blocking-write s start end)]
	      [(stuck)
	       ;; We need more bytes to make progress.
	       ;;  Add out-bytes and s into one string for non-blocking-write.
	       (let ([s2 (bytes-append (subbytes out-bytes out-start out-end)
				       (subbytes s start end))]
		     [out-len (- out-end out-start)])
		 (let ([c (non-blocking-write s2 0 (bytes-length s2))])
		   (and c
			(begin
			  (set! out-start 0)
			  (set! out-end 0)
			  (- c out-len)))))])]
	   [else
	    (when (or (> ready-end ready-start)
		      (< (- (bytes-length out-bytes) out-end) 100))
	      ;; Make room for conversion.
	      (flush-some #f enable-break?))
	    ;; Make room in buffer
	    (when (positive? out-start)
	      (bytes-copy! out-bytes 0 out-bytes out-start out-end)
	      (set! out-end (- out-end out-start))
	      (set! out-start 0))
	    ;; Buffer some bytes:
	    (let ([cnt (min (- end start)
			    (- (bytes-length out-bytes) out-end))])
	      (if (zero? cnt)
		  ;; No room --- try flushing again:
		  (write-it s start end #f enable-break?)
		  ;; Buffer and report success:
		  (begin
		    (bytes-copy! out-bytes out-end s start (+ start cnt))
		    (set! out-end (+ cnt out-end))
		    (case buffer-mode
		      [(none) (flush-all-now enable-break?)]
		      [(line) (when (regexp-match-positions #rx#"[\r\n]" s start (+ start cnt))
				(flush-all-now enable-break?))])
		    cnt)))]))
	   
	(define (non-blocking-write s start end)
	  ;; For now, everything that we can flushed is flushed.
	  ;;  Try to write the minimal number of bytes, and hope for the
	  ;;  best. If none of all of the minimal bytes get written,
	  ;;  everyone is happy enough. If some of the bytes get written,
	  ;;  the we will have buffered bytes when we shouldn't have.
	  ;;  That probably won't happen, but we can't guarantee it.
	  (let loop ([len 1])
	    (let-values ([(got-c used-c status) (bytes-convert c s start (+ start len) ready-bytes)])
	      (cond
	       [(positive? got-c)
		(try-flush-ready got-c used-c)]
	       [(eq? status 'aborts)
		(if (< len (- end start))
		    ;; Try converting a bigger chunk
		    (loop (add1 len))
		    ;; We can't flush half an encoding, so just buffer it.
		    (let ([cnt (- start end)])
		      (when (> (- end start) (bytes-length out-bytes))
			(raise-insane-decoding-length))
		      (bytes-copy out-bytes 0 s start end)
		      (set! out-start 0)
		      (set! out-end cnt)
		      cnt))]
	       [(eq? status 'continues)
		;; Not enough room in ready-bytes!? We give up.
		(raise-insane-decoding-length)]
	       [else
		;; Encoding error. Try to flush error bytes.
		(let ([cnt (bytes-length error-bytes)])
		  (bytes-copy! ready-bytes 0 error-bytes)
		  (try-flush-ready cnt 1))]))))

	(define (write-special-it v no-buffer&block? enable-break?)
	  (cond
	   [(buffer-flushed?)
	    ((if no-buffer&block?
		 write-special-avail*
		 (if enable-break?
		     (lambda (v p)
		       (parameterize-break #t (write-special v p)))
		     write-special))
	     v port)]
	   [else
	    ;; Note: we could get stuck because only half an encoding
	    ;;  is available in out-bytes.
	    (flush-some no-buffer&block? enable-break?)
	    (if (or (buffer-flushed?)
		    (not no-buffer&block?))
		(write-special-it v no-buffer&block? enable-break?)
		#f)]))
	
	;; flush-all : -> 'done, 'not-done, or 'stuck
	(define (flush-all non-block? enable-break?)
	  (let ([orig-none-ready? (= ready-start ready-end)]
		[orig-out-start out-start]
		[orig-out-end out-end])
	    (flush-some non-block? enable-break?)
	    (if (buffer-flushed?)
		'done
		;; Couldn't flush everything. One possibility is that we need
		;;  more bytes to convert before a flush.
		(if (and orig-none-ready?
			 (= ready-start ready-end)
			 (= orig-out-start out-start)
			 (= orig-out-end out-end))
		    'stuck
		    'not-done))))

	(define (flush-all-now enable-break?)
	  (case (flush-all #f enable-break?)
	    [(not-done) (flush-all-now enable-break?)]))
	
	(define (buffer-flushed?)
	  (and (= ready-start ready-end)
	       (= out-start out-end)))
	
	;; Try to flush immediately a certain number of bytes
	(define (try-flush-ready got-c used-c)
	  (let ([c (write-bytes-avail* ready-bytes port 0 got-c)])
	    (if (zero? c)
		;; Didn't flush any - give up:
		#f
		;; Hopefully, we flushed them all, but set ready-start and ready-end,
		;;  just in case.
		(begin	    
		  (set! ready-start c)
		  (set! ready-end got-c)
		  used-c))))

	;; Try to make progress flushing buffered bytes
	(define (flush-some non-block? enable-break?)
	  (unless (= ready-start ready-end)
	    ;; Flush converted bytes:
	    (let ([cnt ((cond
			 [non-block? write-bytes-avail*]
			 [enable-break? write-bytes-avail/enable-break]
			 [else write-bytes-avail])
			ready-bytes port ready-start ready-end)])
	      (set! ready-start (+ ready-start cnt))))
	  (when (= ready-start ready-end)
	    ;; Convert more, if available:
	    (set! ready-start 0)
	    (set! ready-end 0)
	    (when (> out-end out-start)
	      (let-values ([(got-c used-c status) (bytes-convert c out-bytes out-start out-end ready-bytes)])
		(set! ready-end got-c)
		(set! out-start (+ out-start used-c))
		(when (and (eq? status 'continues)
			   (zero? used-c))
		  ;; Yikes! Size of ready-bytes isn't enough room for progress!?
		  (raise-insane-decoding-length))
		(when (and (eq? status 'error)
			   (zero? used-c))
		  ;; No progress before an encoding error.
		  (if error-bytes
		      ;; Write error bytes and drop an output byte:
		      (begin
			(set! out-start (add1 out-start))
			(bytes-copy! ready-bytes 0 error-bytes)
			(set! ready-end (bytes-length error-bytes)))
		      ;; Raise an exception:
		      (begin
			(set! out-start (add1 out-start))
			(error
			 'reencode-output-port
			 "error decoding output to stream: ~e"
			 port))))))))

	;; This error is used when decoding wants more bytes to make progress even
	;;  though we've supplied hundreds of bytes
	(define (raise-insane-decoding-length)
	  (error 'reencode-output-port-write
		 "unable to make decoding progress: ~e"
		 port))

	;; Check that a decoder is available:
	(unless c
	  (error 'reencode-output-port 
		 "could not create converter from ~e to UTF-8"
		 encoding))

	(make-output-port
	 name
	 port
	 write-it
	 (lambda ()
	   ;; Flush output
	   (write-it #"" 0 0 #f #f)
	   (when close?
	     (close-output-port port))
	   (bytes-close-converter c))
	 write-special-it
	 #f #f
	 #f void
	 1
	 (case-lambda
	  [() buffer-mode]
	  [(mode) (let ([old buffer-mode])
		    (set! buffer-mode mode)
		    (when (or (and (eq? old 'block)
				   (memq mode '(none line)))
			      (and (eq? old 'line)
				   (memq mode '(none))))
		      ;; Flush output
		      (write-it #"" 0 0 #f #f)))])))))
  
  (provide open-output-nowhere
	   make-pipe-with-specials
	   make-input-port/read-to-peek
	   peeking-input-port
	   relocate-input-port
	   transplant-input-port
	   relocate-output-port
	   transplant-output-port
	   merge-input
	   copy-port
	   input-port-append
	   convert-stream
	   make-limited-input-port
	   reencode-input-port
	   reencode-output-port
	   strip-shell-command-start)
  
  (provide/contract (read-bytes-avail!-evt (mutable-bytes? input-port-with-progress-evts?
							   . -> . evt?))
		    (peek-bytes-avail!-evt (mutable-bytes? exact-non-negative-integer? evt?/false
							   input-port-with-progress-evts? 
							   . -> . evt?))
		    (read-bytes!-evt (mutable-bytes? input-port-with-progress-evts? . -> . evt?))
		    (peek-bytes!-evt (mutable-bytes? exact-non-negative-integer? evt?/false
						     input-port-with-progress-evts? 
						     . -> . evt?))
		    (read-bytes-evt (exact-non-negative-integer? input-port-with-progress-evts? 
								 . -> . evt?))
		    (peek-bytes-evt (exact-non-negative-integer? exact-non-negative-integer? evt?/false
								 input-port-with-progress-evts? 
								 . -> . evt?))
		    (read-string!-evt (mutable-string? input-port-with-progress-evts? 
						       . -> . evt?))
		    (peek-string!-evt (mutable-string? exact-non-negative-integer? evt?/false
						       input-port-with-progress-evts? 
						       . -> . evt?))
		    (read-string-evt (exact-non-negative-integer? input-port-with-progress-evts? 
								  . -> . evt?))
		    (peek-string-evt (exact-non-negative-integer? exact-non-negative-integer? evt?/false
								  input-port-with-progress-evts? 
								  . -> . evt?))
		    (regexp-match-evt ((or/c regexp? byte-regexp? string? bytes?)
				       input-port-with-progress-evts? 
				       . -> . evt?))

		    (read-bytes-line-evt (case->
					  (input-port-with-progress-evts? . -> . evt?)
					  (input-port-with-progress-evts? line-mode-symbol? . -> . evt?)))
		    (read-line-evt (case->
				    (input-port-with-progress-evts? . -> . evt?)
				    (input-port-with-progress-evts? line-mode-symbol? . -> . evt?)))
		    (eof-evt (input-port-with-progress-evts? . -> . evt?))))
