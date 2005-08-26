(module dispatch-passwords mzscheme
  (require "dispatch.ss"
           "util.ss"
           "servlet-helpers.ss"
           "connection-manager.ss"
           "response.ss"
           "configuration-structures.ss")
  
  (provide interface-version
           gen-dispatcher)
  
  (define interface-version 'v1)
  (define (gen-dispatcher host-info config:access)
    (lambda (conn req)
      (let-values ([(uri method path) (decompose-request req)])
        (cond
          [(access-denied? method path (request-headers req) host-info config:access)
           => (lambda (realm)
                (adjust-connection-timeout! conn (timeouts-password (host-timeouts host-info)))
                (request-authentication conn method uri host-info realm))]
          [(string=? "/conf/refresh-passwords" path)
           ;; more here - send a nice error page
           (hash-table-put! config:access host-info
                            (read-passwords host-info))
           (output-response/method
            conn
            ((responders-passwords-refreshed (host-responders host-info)))
            method)]
          [else
           (next-dispatcher)]))))
  
  ;; ****************************************
  ;; ****************************************
  ;; ACCESS CONTROL
  
  ;; pass-entry = (make-pass-entry str regexp (list sym str))
  (define-struct pass-entry (domain pattern users))
  
  ;; access-denied? : Method string x-table host Access-table -> (+ false str)
  ;; the return string is the prompt for authentication
  (define (access-denied? method uri-str headers host-info access-table)
    ;; denied?: str sym str -> (U str #f)
    ;; a function to authenticate the user
    (let ([denied?
           
           ;; GregP lookup the authenticator function, if you can't find it, then try to load the
           ;; passwords file for this host.
           (hash-table-get
            access-table host-info
            (lambda ()
              ; more here - a malformed password file will kill the connection
              (let ([f (read-passwords host-info)])
                (hash-table-put! access-table host-info f)
                f)))])
      (let ([user-pass (extract-user-pass headers)])
        (if user-pass
            (denied? uri-str (lowercase-symbol! (car user-pass)) (cdr user-pass))
            (denied? uri-str fake-user "")))))
  
  (define-struct (exn:password-file exn) ())
  
  ;; : host -> (str sym str -> (U str #f))
  ;; to produce a function that checks if a given url path is accessible by a given user with a given
  ;; password.  If not, the produced function returns a string, prompting for the password.
  ;; If the password file does not exist, all accesses are allowed.  If the file is malformed, an
  ;; exn:password-file is raised.
  (define (read-passwords host-info)
    (let ([password-path (host-passwords host-info)])
      (with-handlers ([void (lambda (exn)
                              (raise (make-exn:password-file (format "could not load password file ~a" password-path)
                                                             (current-continuation-marks))))])
        (if (and (file-exists? password-path) (memq 'read (file-or-directory-permissions password-path)))
            (let ([passwords
                   (let ([raw (load password-path)])
                     (unless (password-list? raw)
                       (raise "malformed passwords"))
                     (map (lambda (x) (make-pass-entry (car x) (regexp (cadr x)) (cddr x)))
                          raw))])
              
              ;; string symbol bytes -> (union #f string)
              (lambda (request-path user-name password)
                (ormap (lambda (x)
                         (and (regexp-match (pass-entry-pattern x) request-path)
                              (let ([name-pass (assq user-name (pass-entry-users x))])
                                (if (and name-pass
                                         (string=?
                                          (cadr name-pass)
                                          (bytes->string/utf-8 password)))
                                    #f
                                    (pass-entry-domain x)))))
                       passwords)))
            (lambda (req user pass) #f)))))
  
  (define fake-user (gensym))
  
  ;; password-list? : TST -> bool
  
  ;; Note: andmap fails for dotted pairs at end.
  ;; This is okay, since #f ends up raising a caught exception anyway.
  (define (password-list? passwords)
    (and (list? passwords)
         (andmap (lambda (domain)
                   (and (pair? domain) (pair? (cdr domain)) (list (cddr domain))
                        (string? (car domain))
                        (string? (cadr domain))
                        (andmap (lambda (x)
                                  (and (pair? x) (pair? (cdr x)) (null? (cddr x))
                                       (symbol? (car x)) (string? (cadr x))))
                                (cddr domain))))
                 passwords)))
  
  ;; request-authentication : connection Method URL iport oport host str bool -> bool
  ;; GregP: at first look, it seems that this gets called when the user
  ;; has supplied bad authentication credentials.
  (define (request-authentication conn method uri host-info realm)
    (output-response/method
     conn
     ((responders-authentication (host-responders host-info))
      uri `(WWW-Authenticate . ,(string-append " Basic
                       realm=\"" realm "\"")))
     method)))