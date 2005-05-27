
(module optionr mzscheme
  (require (lib "unitsig.ss")
	   (lib "string.ss"))

  (require (lib "imap-sig.ss" "net")
           (lib "mred-sig.ss" "mred")
	   (lib "framework.ss" "framework"))

  (require "sirmails.ss"
	   "pref.ss")

  (define shared-password #f)

  ;; The option@ unit gets instanted afresh for every window, but
  ;; it defers practically all of its work to the "pref.ss" module
  ;; (which is only instantiated once).
  
  (provide option@)
  (define option@
    (unit/sig sirmail:options^
      (import sirmail:environment^
	      net:imap^
              mred^)

      (define (parse-server-name s default-port)
	(let ([m (regexp-match "^([^:]*):([^:]*)$" s)])
	  (if (and m (string->number (caddr m)))
	      (values (cadr m) (string->number (caddr m)))
	      (values s default-port))))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Preferences                                            ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


      (define (MAIL-FROM) (get-pref 'sirmail:mail-from))
      (define (USERNAME) (get-pref 'sirmail:username))
      (define (DEFAULT-DOMAIN) (get-pref 'sirmail:default-to-domain))
      (define (IMAP-SERVER) (get-pref 'sirmail:imap-server))
      (define (LOCAL-DIR) (get-pref 'sirmail:local-directory))
      (define (SAVE-SENT) (get-pref 'sirmail:sent-directory))

      (define (SMTP-SERVERS) (let ([s (get-pref 'sirmail:smtp-server)])
			       (regexp-split ", *" s)))
      (define current-SMTP-SERVER (car (SMTP-SERVERS)))
      (define (SMTP-SERVER) (let ([l (SMTP-SERVERS)])
			      (if (member current-SMTP-SERVER l)
				  current-SMTP-SERVER
				  (car l))))
      (define (set-SMTP-SERVER! s) (set! current-SMTP-SERVER s))


      (define PASSWORD (get-pref 'sirmail:password))
      (define (get-PASSWORD) (or PASSWORD shared-password))
      (define (set-PASSWORD p) (set! shared-password p))

      (define (BIFF-DELAY) (get-pref 'sirmail:biff-delay))

      (define (ALIASES) (let ([f (get-pref 'sirmail:aliases-file)])
			  (with-handlers ([exn:fail? (lambda (x) null)])
			    (with-input-from-file f read))))

      (define (SELF-ADDRESSES) (get-pref 'sirmail:self-addresses))

      (define (AUTO-FILE-TABLE) (let ([f (get-pref 'sirmail:auto-file-table-file)])
				  (and f
				       (with-handlers ([exn:fail? (lambda (x) null)])
					 (with-input-from-file f read)))))

      (define (SORT) (get-pref 'sirmail:initial-sort))

      (define (MESSAGE-FIELDS-TO-SHOW) (get-pref 'sirmail:fields-to-show))

      (define (ROOT-MAILBOX-FOR-LIST) (get-pref 'sirmail:root-mailbox-folder))
      (define (ARCHIVE-MAILBOX) (get-pref 'sirmail:archive-mailbox-folder))

      (define (USE-EXTERNAL-COMPOSER?) (get-pref 'sirmail:use-extenal-composer?))

      (define (WARN-DOWNLOAD-SIZE) (get-pref 'sirmail:warn-download-size))
      
      (define (SHOW-URLS) (get-pref 'sirmail:show-urls?)))))