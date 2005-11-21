(module dispatch-passwords mzscheme
  (require "dispatch.ss"
           "util.ss"
           "servlet-helpers.ss"
           "connection-manager.ss"
           "response.ss")
  
  (provide interface-version
           gen-dispatcher)
  
  (define interface-version 'v1)
  (define (gen-dispatcher password-file password-connection-timeout authentication-responder passwords-refresh-responder)
    (let* ([password-cache (box #f)]
           [reset-password-cache!
            (lambda ()
              ; more here - a malformed password file will kill the connection
              (set-box! password-cache (read-passwords password-file)))]
           [read-password-cache
            (lambda ()
              (unbox password-cache))])
      (reset-password-cache!)
      (lambda (conn req)
        (let-values ([(uri method path) (decompose-request req)])
          (cond
            [(access-denied? method path (request-headers req) (read-password-cache))
             => (lambda (realm)
                  (adjust-connection-timeout! conn password-connection-timeout)
                  (request-authentication conn method uri
                                          authentication-responder
                                          realm))]
            [(string=? "/conf/refresh-passwords" path)
             ;; more here - send a nice error page
             (reset-password-cache!)
             (output-response/method
              conn
              (passwords-refresh-responder)
              method)]
            [else
             (next-dispatcher)])))))
  
  ;; ****************************************
  ;; ****************************************
  ;; ACCESS CONTROL
  
  ;; pass-entry = (make-pass-entry str regexp (list sym str))
  (define-struct pass-entry (domain pattern users))
  
  ;; access-denied? : Method string x-table denied? -> (+ false str)
  ;; denied?: str sym str -> (U str #f)
  ;; the return string is the prompt for authentication
  (define (access-denied? method uri-str headers denied?)    
    (let ([user-pass (extract-user-pass headers)])
      (if user-pass
          (denied? uri-str (lowercase-symbol! (car user-pass)) (cdr user-pass))
          (denied? uri-str fake-user ""))))
  
  (define-struct (exn:password-file exn) ())
  
  ;; : host -> (str sym str -> (U str #f))
  ;; to produce a function that checks if a given url path is accessible by a given user with a given
  ;; password.  If not, the produced function returns a string, prompting for the password.
  ;; If the password file does not exist, all accesses are allowed.  If the file is malformed, an
  ;; exn:password-file is raised.
  (define (read-passwords password-path)
    (with-handlers ([void (lambda (exn)
                            (raise (make-exn:password-file (string->immutable-string
                                                            (format "could not load password file ~a" password-path))
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
          (lambda (req user pass) #f))))
  
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
  (define (request-authentication conn method uri authentication-responder realm)
    (output-response/method
     conn
     (authentication-responder
      uri 
      `(WWW-Authenticate . ,(format " Basic realm=\"~a\"" realm)))
     method)))