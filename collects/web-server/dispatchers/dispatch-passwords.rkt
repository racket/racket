#lang racket/base
(require racket/list
         net/url
         racket/contract)
(require web-server/dispatchers/dispatch
         web-server/private/util
         web-server/configuration/responders
         web-server/http
         web-server/http/response)

(define denied?/c (request? . -> . (or/c false/c string?)))
(define authorized?/c (string? (or/c false/c bytes?) (or/c false/c bytes?) . -> . (or/c false/c string?)))

(provide/contract
 [interface-version dispatcher-interface-version/c]
 [denied?/c contract?]
 [make (->* (denied?/c)
            (#:authentication-responder
             (url? header? . -> . response?))
            dispatcher/c)]
 [authorized?/c contract?]
 [make-basic-denied?/path
  (authorized?/c . -> . denied?/c)]
 [password-file->authorized?
  (path-string? . -> . (values (-> void)
                               authorized?/c))])

(define interface-version 'v1)
(define (make denied?
              #:authentication-responder 
              [authentication-responder 
               (gen-authentication-responder "forbidden.html")])
  (lambda (conn req)
    (define uri (request-uri req))
    (define method (request-method req))
    (cond
      [(denied? req)
       => (lambda (realm)
            (request-authentication conn method uri
                                    authentication-responder
                                    realm))]
      [else
       (next-dispatcher)])))

(define (make-basic-denied?/path
         authorized?)  
  (lambda (req)
    (define path (url-path->string (url-path (request-uri req))))
    (cond
      [(request->basic-credentials req)
       => (lambda (user*pass)
            (authorized? path 
                         (car user*pass)
                         (cdr user*pass)))]
      [else
       (authorized? path #f #f)])))

(define (password-file->authorized? password-file)
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
  (values update-password-cache!
          (lambda (path user pass)
            (define denied? (read-password-cache))
            (if denied?
                (denied? path (if user (lowercase-symbol! user) #f) pass)
                ; Fail un-safe
                #f))))

;; pass-entry = (make-pass-entry str regexp (list sym str))
(define-struct pass-entry (domain pattern users))

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
           (with-input-from-file
               password-path
             (lambda ()
               (let ([raw (second (read))])
                 (unless (password-list? raw)
                   (raise "malformed passwords"))
                 (map (lambda (x) (make-pass-entry (car x) (regexp (cadr x)) (cddr x)))
                      raw))))])
      
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
    (make-basic-auth-header realm))
   method))
