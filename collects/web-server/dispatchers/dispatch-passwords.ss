(module dispatch-passwords mzscheme
  (require (lib "kw.ss")
           (lib "url.ss" "net")
           (lib "contract.ss"))
  (require "dispatch.ss"
           "../private/util.ss"
           "../configuration/responders.ss"
           "../private/request-structs.ss"
           "../servlet/basic-auth.ss"
           "../private/response.ss")  
  (provide/contract
   [interface-version dispatcher-interface-version?])
  (provide make)
  
  (define interface-version 'v1)
  (define/kw (make #:key
                   ; XXX Take authorized? function
                   [password-file "passwords"]
                   [authentication-responder 
                    (gen-authentication-responder "forbidden.html")])
    (define last-read-time (box #f))
    (define password-cache (box #f))
    (define (update-password-cache!)
      (when (and (file-exists? password-file) (memq 'read (file-or-directory-permissions password-file)))
          (let ([cur-mtime (file-or-directory-modify-seconds password-file)])
            (when (or (not (unbox last-read-time))
                      (cur-mtime . > . (unbox last-read-time))
                      (not (unbox password-cache)))                
              (set-box! last-read-time cur-mtime)
              (set-box! password-cache (read-passwords password-file))))))
    (define (read-password-cache)
      (update-password-cache!)
      (unbox password-cache))
    (define (dispatch conn req)
      (define uri (request-uri req))
      (define path (url-path->string (url-path uri)))
      (define method (request-method req))
      (define denied? (read-password-cache))
      (cond
        [(and denied?
              (access-denied? method path (request-headers/raw req) denied?))
         => (lambda (realm)
              (request-authentication conn method uri
                                      authentication-responder
                                      realm))]
        [else
         (next-dispatcher)]))
    (values update-password-cache!
            dispatch))
  
  ;; ****************************************
  ;; ****************************************
  ;; ACCESS CONTROL
  
  ;; pass-entry = (make-pass-entry str regexp (list sym str))
  (define-struct pass-entry (domain pattern users))
  
  ;; access-denied? : Method string x-table denied? -> (or/c false str)
  ;; denied?: str sym str -> (or/c str #f)
  ;; the return string is the prompt for authentication
  (define (access-denied? method uri-str headers denied?)
    (define user-pass (extract-user-pass headers))
    (if user-pass
        (denied? uri-str (lowercase-symbol! (car user-pass)) (cdr user-pass))
        (denied? uri-str fake-user "")))
  
  (define-struct (exn:password-file exn) ())
  
  ;; : host -> (str sym str -> (or/c str #f))
  ;; to produce a function that checks if a given url path is accessible by a given user with a given
  ;; password.  If not, the produced function returns a string, prompting for the password.
  ;; If the password file does not exist, all accesses are allowed.  If the file is malformed, an
  ;; exn:password-file is raised.
  (define (read-passwords password-path)
    (with-handlers ([void (lambda (exn)
                            (raise (make-exn:password-file
                                    (format "could not load password file ~a" password-path)
                                    (current-continuation-marks))))])
      (let ([passwords
             (let ([raw (load password-path)])
               (unless (password-list? raw)
                 (raise "malformed passwords"))
               (map (lambda (x) (make-pass-entry (car x) (regexp (cadr x)) (cddr x)))
                    raw))])
        
        ;; string symbol bytes -> (or/c #f string)
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
                 passwords)))))
  
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
      (make-header #"WWW-Authenticate" (string->bytes/utf-8 (format " Basic realm=\"~a\"" realm))))
     method)))