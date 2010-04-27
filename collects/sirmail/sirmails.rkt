
(module sirmails mzscheme
  (require mzlib/unit)
  
  (provide sirmail:exit^
           sirmail:environment^)
  
  (define-signature sirmail:exit^
    (exit-sirmail))

  (define-signature sirmail:environment^ extends sirmail:exit^
    (mailbox-name 
     mailbox-options 
     open-folders-window
     get-active-folder

     open-mailbox
     start-new-window))

  (provide sirmail:utils^)
  (define-signature sirmail:utils^
    (crlf
     split
     splice
     split-crlf
     split-crlf/preserve-last
     split-lf
     crlf->lf
     crlf->lf/preserve-last
     lf->crlf
     string-crlf->lf
     string-lf->crlf
     header->lines
     enumerate
     find
     string->regexp

     show-error-message-box

     as-background

     make-fixed-width

     confirm-box

     get-pw-from-user

     generalize-encoding
     parse-encoded
     encode-for-header))

  (provide sirmail:send^)
  (define-signature sirmail:send^
    (new-mailer
     (struct enclosure (name subheader data-thunk))))

  (provide sirmail:options^)
  (define-signature sirmail:options^
    (IMAP-SERVER
     USERNAME
     get-PASSWORD
     set-PASSWORD

     LOCAL-DIR

     MAIL-FROM
     ALIASES
     DEFAULT-DOMAIN
     SMTP-SERVER
     SMTP-SERVERS
     set-SMTP-SERVER!
     SAVE-SENT

     ROOT-MAILBOX-FOR-LIST
     ARCHIVE-MAILBOX
     
     MESSAGE-FIELDS-TO-SHOW
     WARN-DOWNLOAD-SIZE
     AUTO-FILE-TABLE
     BIFF-DELAY
     SELF-ADDRESSES
     SORT
     SHOW-URLS
     
     USE-EXTERNAL-COMPOSER?

     parse-server-name
     parse-server-name+user+type))

  (provide sirmail:read^)
  (define-signature sirmail:read^
    (queue-directory))

  (provide sirmail:shutdown-folder^)
  (define-signature sirmail:shutdown-folder^
    (shutdown-folders-window)))
