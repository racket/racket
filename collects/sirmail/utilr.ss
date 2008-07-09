
(module utilr mzscheme
  (require mzlib/unit
	   mzlib/class
	   mred/mred-sig
	   net/qp-sig
	   net/base64-sig
	   (prefix unihead: net/unihead)
	   mzlib/etc
	   mzlib/string)

  (require "sirmails.ss")

  (provide util@)
  (define-unit util@
      (import mred^
	      base64^
	      qp^)
      (export sirmail:utils^)

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Utilities                                              ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define crlf (string #\return #\linefeed))

      (define (split s re)
	(regexp-split re s))

      (define (drop-last-if-empty orig-l)
	(let loop ([l orig-l][accum null])
	  (cond
	   [(null? l) orig-l]
	   [(null? (cdr l))
	    (if (equal? #"" (car l))
		(reverse accum)
		orig-l)]
	   [else (loop (cdr l) (cons (car l) accum))])))

      (define (splice l sep)
	(if (null? l)
	    #""
	    (let ([p (open-output-bytes)])
	      (let loop ([l l])
		(write-bytes (car l) p)
		(unless (null? (cdr l))
		  (display sep p)
		  (loop (cdr l))))
	      (get-output-bytes p))))
      
      (define (split-crlf/preserve-last s)
	(split s #rx#"\r\n"))

      (define (split-crlf s)
	(drop-last-if-empty (split-crlf/preserve-last s)))

      (define (split-lf s)
	(drop-last-if-empty (split s #rx#"\n")))

      (define (crlf->lf s)
	(splice (split-crlf s) #"\n"))

      (define (crlf->lf/preserve-last s)
	(splice (split-crlf/preserve-last s) #"\n"))

      (define (lf->crlf s)
	(splice (split-lf s) #"\r\n"))

      (define (string-crlf->lf s)
	(regexp-replace* #rx"\r\n" s "\n"))

      (define (string-lf->crlf s)
	(regexp-replace* #rx"\n" s "\r\n"))

      (define (header->lines s)
	(regexp-split #rx"\r\n" 
		      ;; We don't want the extra empty line at the end:
		      (substring s 0 (- (string-length s) 2))))

      (define (enumerate n)
	(let loop ([n n][a null])
	  (if (zero? n)
	      a
	      (loop (sub1 n) (cons n a)))))

      (define (find i l)
	(let loop ([l l][pos 0])
	  (if (null? l)
	      #f
	      (if (eq? (car l) i)
		  pos
		  (loop (cdr l) (add1 pos))))))

      (define (string->regexp s)
	(regexp-quote s))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (show-error-message-box x main-frame)
	(let ([sp (open-output-string)])
	  ;; use error display handler in case
	  ;; errortrace (or something else) is
	  ;; installed
	  (parameterize ([current-output-port sp]
			 [current-error-port sp])
	    ((error-display-handler)
	     (if (exn? x)
		 (exn-message x)
		 (format "uncaught exn: ~s" x))
	     x))
	  (message-box "Error" 
		       (get-output-string sp)
		       main-frame
		       '(ok stop))))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (break-really-hard? set-d!)
	(let* ([d (make-object dialog% "Danger")]
	       [p (make-object vertical-pane% d)]
	       [bp (make-object horizontal-pane% d)]
	       [result #f])
	  (send bp stretchable-width #f)
	  (send bp stretchable-height #f)
	  (make-object message% "Breaking now is dangerous." p)
	  (make-object message% "It requires killing the window." p)
	  (make-object message% "" p)
	  (make-object message% "Are you sure you want to kill?" p)
	  (make-object button% "&Kill" bp (lambda (b e)
					    (set! result #t)
					    (send d show #f)))
	  (make-object button% "Cancel" bp (lambda (b e) (send d show #f)))
	  (set-d! d)
	  (send d show #t)
	  result))

      ;; as-background: (bool window<%> braek-thunk ->)
      ;;                (break-ok-thunk break-not-ok-thunk -> X) 
      ;;                (-> Y) -> X
      ;; Runs a task in the background.
      ;; The `enable' function is called with #f as the first argument
      ;;  near the start of the background task. The `break-thunk' can be
      ;;  called to interrupt the task. The `enable' function might
      ;;  not get called at all if the task is fast enough.
      ;; The `go' function performs the task (in a thread created by
      ;;   as-background); it receives thunks that it can call to
      ;;   indicate when breaks are "safe". If the user tries to break
      ;;   at a non-safe point, the user is warned; if the user
      ;;   proceeds, things are killed and `exit' is called. If
      ;;   the user breaks at a safe point, a break signal is sent
      ;;   to the thread for the background task.
      ;; The `pre-kill' thunk is called before things are killed
      ;;   for a non-"safe" break.
      (define (as-background enable go pre-kill)
	(let* ([v #f]
	       [exn #f]
	       [break-ok? #f]
	       [breaking-dialog #f]
	       [adjust-break (make-semaphore 1)]
	       [change-break-ok (lambda (ok?)
				  (lambda ()
				    (semaphore-wait adjust-break)
				    (set! break-ok? ok?)
				    (let ([waiting? (and ok? breaking-dialog)])
				      (when waiting?
					(send breaking-dialog show #f)
					(set! breaking-dialog #f))
				      (semaphore-post adjust-break)
				      (when waiting?
					(break-thread (current-thread))))))]
	       [s (make-semaphore 0)]
	       [t (thread (lambda ()
			    (with-handlers ([void (lambda (x) 
						    (set! exn x))])
			      (set! v (call-with-values 
					  (lambda () (go (change-break-ok #f)
							 (change-break-ok #t)))
					list))
			      ((change-break-ok #f)))
			    (when breaking-dialog
			      (send breaking-dialog show #f))
			    (semaphore-post s)))])
	  ;; If the operation is fast enough, no need to disable then yield then enable,
	  ;; which makes the screen flash and causes events to get dropped. 1/4 second
	  ;; seems "fast enough".
	  (unless (sync/timeout 0.25 s)
	    (let ([v (enable #f #f
			     (lambda ()
			       (semaphore-wait adjust-break)
			       (if break-ok?
				   (break-thread t)
				   (let ([v (break-really-hard? (lambda (d) 
								  (set! breaking-dialog d)
								  (semaphore-post adjust-break)))])
				     (semaphore-wait adjust-break)
				     (set! breaking-dialog #f)
				     (semaphore-post adjust-break)
				     (when v
				       (pre-kill)
				       (kill-thread t)
				       (exit))))))])
	      (yield s)
	      (enable #t v void)))
	  (if exn
	      (raise exn)
	      (apply values v))))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; the actual fixed width font is already set by
      ;; the editor:standard-style-list-mixin
      (define (make-fixed-width c e wrap? wrap-bm)
	(let ([s (send (send e get-style-list)
		       find-named-style "Standard")])
	  (send e set-tabs null 8 #f)
	  (let ([font (send s get-font)]
		[dc (send c get-dc)]
		[wbox (box 0)]
		[hbox (box 0)])
	    (send e get-view-size wbox hbox)
	    (let-values ([(w h) (send c get-size)]
			 [(1w 1h d a) (send dc get-text-extent "X" font)])
	      (let ([80chars (+ (* 1w 80)
				2 ; +2 for caret
				(if wrap-bm 
				    (send wrap-bm get-width) 
				    0))])
		(when wrap? 
		  (when wrap-bm 
		    (send e set-autowrap-bitmap wrap-bm))
		  (send e set-max-width 80chars))
		(send c min-width 
		      (inexact->exact (round (+ 80chars (- w (unbox wbox)))))))))))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define confirm-box 
	(opt-lambda (title message [parent #f] [style null])
	  (if (= 1 (message-box/custom
		    title
		    message
		    "&Yes"
		    "&No"
		    #f
		    parent
		    (append (if (memq 'app style) null '(caution)) 
			    '(default=1))
		    2))
	      'yes
	      'no)))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (get-pw-from-user username parent)
	(get-text-from-user "Password" 
			    (format "Password for ~a:" username)
			    parent
			    ""
			    '(password)))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Decoding `from' names                                  ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define generalize-encoding unihead:generalize-encoding)
      (define parse-encoded unihead:decode-for-header)
      (define encode-for-header unihead:encode-for-header)))
