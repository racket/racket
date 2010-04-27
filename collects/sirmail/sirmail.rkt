;; SirMail: Simple Imap Reader for Mail
;;          (with a mail composer, too)

(module sirmail mzscheme
  (require mzlib/unit
	   mzlib/class
	   mred/mred-sig
	   mred
           framework
	   mzlib/list)

  (require "sirmails.ss"
	   "pref.ss"
	   "sirmailr.ss")

  (require net/imap-sig
	   net/smtp-sig
	   net/head-sig
	   net/base64-sig
	   net/mime-sig
	   net/qp-sig
	   net/imap
	   net/smtp
	   net/head
	   net/base64
	   net/mime
	   net/qp)

  (require mrlib/hierlist/hierlist-sig
	   mrlib/hierlist)

  ;; For testing purposes, cause GC accounting to be
  ;;  enabled:
  (current-memory-use (current-custodian))

  ;; Constants:
  (define inbox-name "Inbox")
  (define default-mailbox-options null)

  ;; ------------------------------------------------------------
  ;; Every window (reader or sender) is in it's own
  ;; eventspace. Each should terminate by calling `exit'.
  ;; We install an exit handler so that we only actually
  ;; exit when the last window is closed.

  (define prim-exit (exit-handler))
  (define exit-eventspaces null)
  (define exit-sema (make-semaphore 1))
  (define (exit-sirmail where)
    (let ([evtsp (current-eventspace)])
      ;; Lock is because a separate process might be calling exit
      ;;  or starting up
      (semaphore-wait exit-sema)
      (set! exit-eventspaces (remq evtsp exit-eventspaces))
      (when (null? exit-eventspaces)
	(prim-exit 0))
      (semaphore-post exit-sema)))

  ;; This function is called to start a new window:
  (define (start-new-window thunk)
    (define evtsp (make-eventspace))
    (parameterize ([current-eventspace evtsp])
      (semaphore-wait exit-sema)
      (set! exit-eventspaces (cons evtsp exit-eventspaces))
      (semaphore-post exit-sema)
      (queue-callback
       (lambda ()
	 (exit-handler (lambda (x) (exit-sirmail "a")))
	 (let ([eeh (error-escape-handler)])
	   (error-escape-handler
	    (lambda () 
	      (unless (pair? (get-top-level-windows))
		;; Didn't start up...
		(exit-sirmail "b"))
	      (eeh))))
	 (thunk)
	 (yield 'wait)
	 (exit-sirmail "c")))))

  ;; Reader windows -----------------------------------------------------------
  
  ;; This function uses `start-new-window' to open a reader window.
  ;; A reader window is implemented by an instance of the sirmail@ unit.
  (define open-mailbox
    (case-lambda
     [(mailbox-name) (open-mailbox mailbox-name default-mailbox-options)]
     [(mailbox-name mailbox-options)
      (start-new-window
       (lambda ()
	 (invoke-unit sirmail@
                      (import sirmail:environment^
                              mred^
                              imap^
                              smtp^
                              head^
                              base64^
                              mime^
                              qp^
                              hierlist^))))]))

  ;; There's only one Folders window ----------------------------------------
  
  (require "optionr.ss"
	   "folderr.ss")

  (define folders-window #f)
  (define folders-lock (make-semaphore 1))
  
  (define (with-folders-lock t)
    (dynamic-wind
     (lambda () (semaphore-wait folders-lock))
     t
     (lambda () (semaphore-post folders-lock))))
  
  (define (open-folders-window)
    (with-folders-lock
     (lambda ()
       (if folders-window
           (send folders-window show #t)
           (let ([shutdown-folders-window
                  (lambda ()
                    (with-folders-lock
                     (lambda ()
                       (set! folders-window #f)
                       (exit-sirmail "d"))))]
                 [mailbox-name inbox-name]
                 [mailbox-options default-mailbox-options])
             (start-new-window
              (lambda ()
                (set! folders-window
                      (let ()
                        (define-compound-unit/infer together@
                          (import [env : sirmail:environment^]
                                  [s : sirmail:shutdown-folder^]
                                  [mred : mred^]
                                  [imap : imap^]
                                  [hierlist : hierlist^])
                          (export)
                          (link option@ folder@))
                        (invoke-unit together@
                                     (import
                                      sirmail:environment^
                                      sirmail:shutdown-folder^
                                      mred^
                                      imap^
                                      hierlist^)))))))))))
  
  (define (get-active-folder)
    (with-folders-lock
     (lambda ()
       (and folders-window
            (send folders-window get-mailbox-name)))))

  ;; Set Quit handler to try closing all windows --------------------

  (define asking-for-quit? #f)

  (application-quit-handler
   (lambda ()
     (if asking-for-quit?
	 (let ([l (get-top-level-windows)])
	   (when (pair? l)
	     ;; Createa thread because it's probably a dialog...
	     (thread (lambda () (send (car l) show #t)))))
	 (dynamic-wind
	     (lambda () (set! asking-for-quit? #t))
	     (lambda ()
	       (when (= 1 (message-box/custom
			   "Confirm Quit"
			   "Really quit?"
			   "Quit" "Cancel" #f
			   #f '(default=1)
			   2))
		 (let ([l (begin
			    (semaphore-wait exit-sema)
			    (begin0
			     exit-eventspaces
			     (semaphore-post exit-sema)))])
		   (for-each
		    (lambda (evtsp)
		      (parameterize ([current-eventspace evtsp])
			(queue-callback
			 (lambda ()
			   (let ([f (get-top-level-edit-target-window)])
			     (when (and f (f . is-a? . frame%))
			       (when (send f can-close?)
				 (send f on-close)
				 (send f show #f)))))
			 #f)))
		    l))))
	     (lambda () (set! asking-for-quit? #f))))))

  ;; We start by opening "Inbox" ----------------------------------------
  
  (open-mailbox inbox-name)

  ;; Wait for an explicit exit
  (yield (make-semaphore)))
