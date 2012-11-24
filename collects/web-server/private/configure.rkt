#lang racket/base
(require web-server/servlet
         net/url
         mzlib/etc
         (except-in racket/list
                    drop)
         racket/pretty
         (only-in racket/file
                  make-directory*)
         web-server/configuration/configuration-table-structs
         web-server/configuration/configuration-table
         web-server/private/util
         racket/runtime-path)

(define-runtime-path default-web-root
  "../default-web-root")

(provide
 interface-version timeout
 start)

(define timeout (* 12 60 60))
(define interface-version 'v1)

;; FIX
; - fuss with changing absolute paths into relative ones internally
; - move old config files instead of copying default ones
;   - ask: - move exisiting (don't move defaults)
;          - copy defaults to new location
;          - use files existing in the new location (ask only when they exist)
;   - do this when either
;     - changing the root dir (and at least one other file depends on it?)
;     - editing an individual path
; - change all configuration paths (in the configure servlet and in the server) to
;   use a platform independent representation (i.e. a listof strings)

; to convert a platform dependent path into a listof path parts such that
; (forall x (equal? (path->list x) (path->list (apply build-path (path->list x)))))
(define (path->list p)
  (let loop ([p p] [acc null])
    (let-values ([(base name must-be-dir?) (split-path p)])
      (let ([new-acc (cons name acc)])
        (cond
          [(string? base) (loop base new-acc)]
          [else ; conflate 'relative and #f
           new-acc])))))

; build-suspender : (listof html) (listof html) [(listof (cons sym str))] [(listof (cons sym str))] -> str -> response
(define build-suspender
  (opt-lambda (title content [body-attributes '([bgcolor "white"])] [head-attributes null])
    (lambda (k-url)
      (response/xexpr
       `(html (head ,head-attributes
                    (meta ([http-equiv "Pragma"] [content "no-cache"])) ; don't cache in netscape
                    (meta ([http-equiv "Expires"] [content "-1"])) ; don't cache in IE
                    ; one site said to use -1, another said to use 0.
                    (title . ,title))
              (body ,body-attributes
                    (form ([action ,k-url] [method "post"])
                          ,@content)))))))

; write-to-file : str TST -> void
(define (write-to-file file-name x)
  (call-with-output-file file-name
    (lambda (out) (pretty-write x out))
    #:exists 'truncate))

(define default-configuration-path default-configuration-table-path)
(define (set-config-path! new)
  (set! default-configuration-path new))

(define CONFIGURE-SERVLET-NAME "configure.rkt")
(define WIDE "70")

; passwords = (listof realm)
; realm = (make-realm str str (listof user-pass))
(define-struct realm (name pattern allowed))

; user-pass = (make-user-pass sym str)
(define-struct user-pass (user pass))

(define doc-dir "Defaults/documentation")

(define edit-host-button-name "Edit Minor Details")

; build-footer : str -> html
(define (build-footer base)
  (let ([scale (lambda (n) (number->string (round (/ n 4))))])
    `(p "Powered by "
        (a ([href "http://racket-lang.org/"]) "Racket"))))

(define footer (build-footer "/"))

; access-error-page : html
(define access-error-page
  (response/xexpr
   `(html (head (title "Web Server Configuration Access Error"))
          (body ([bgcolor "white"])
                (p "You must connect to the configuration tool from the machine the server runs on using 127.0.0.1 for the host part of the URL.")
                ,footer))))

; permission-error-page : path -> html
(define (permission-error-page configuration-path)
  (response/xexpr
   `(html (head (title "Web Server Configuration Permissions Error"))
          (body ([bgcolor "white"])
                (p "You must have read and write access to "
                   (code ,(path->string configuration-path))
                   " in order to configure the server.")))))

; check-ip-address : request -> request
(define (check-ip-address request)
  (unless (string=? "127.0.0.1" (request-host-ip request))
    (send/finish access-error-page))
  request)

(define web-base (directory-part default-configuration-path))

; interact : (str -> response) -> bindings
(define (interact page)
  (request-bindings (check-ip-address (send/suspend page))))

; choose-configuration-file : -> doesn't
(define (choose-configuration-file)
  (let ([configuration-path (ask-for-configuration-path)])
    (let loop ()
      (if (file-exists? configuration-path)
          (let ([perms (file-or-directory-permissions configuration-path)])
            ; race condition - changing the permissions after the check
            ; will result in an exception later (which serves them right)
            (if (and (memq 'write perms) (memq 'read perms))
                (configure-top-level configuration-path)
                (send/finish (permission-error-page configuration-path))))
          (begin (send/suspend (copy-configuration-file configuration-path))
                 (with-handlers ([exn:fail:filesystem:exists? send-exn])
                   (let-values ([(base name must-be-dir) (split-path configuration-path)])
                     (ensure-directory-shallow base))
                   (copy-file default-configuration-path configuration-path))
                 (loop))))))

; copy-configuration-file : path -> html
(define (copy-configuration-file configuration-path)
  (build-suspender
   '("Copy Configuration File")
   `((h1 "Copy Configuration File")
     (p "The configuration file "
        (blockquote (code ,(path->string configuration-path)))
        "does not exist.  Would you like to copy the default configuration to this "
        "location?")
     (center (input ([type "submit"] [name "ok"] [value "Copy"]))))))

; ask-for-configuration-path : -> path
(define (ask-for-configuration-path)
  (build-path
   (extract-binding/single
    'path
    (request-bindings (send/suspend configuration-path-page)))))

; configuration-path-page : str -> html
(define configuration-path-page
  (build-suspender
   '("Choose a Configuration File")
   `((h1 "Choose a Web Server Configuration File")
     (p "Choose a Web server configuration file to edit. "
        (br)
        "This Web server uses the configuration in "
        (blockquote (code ,(path->string default-configuration-path))))
     (table (tr (th "Configuration path")
                (td (input ([type "text"] [name "path"] [size ,WIDE]
                                          [value ,(path->string default-configuration-path)]))))
            (tr (td ([colspan "2"] [align "center"])
                    (input ([type "submit"] [name "choose-path"] [value "Select"]))))))))

; configure-top-level : path -> doesn't
(define (configure-top-level configuration-path)
  (with-handlers ([exn:fail:filesystem:exists? send-exn])
    (let ([original-configuration (read-configuration configuration-path)])
      (let loop ([configuration original-configuration])
        (let* ([update-bindings (interact (request-new-configuration-table configuration original-configuration))]
               [form-configuration
                (delete-hosts (update-table configuration update-bindings)
                              (foldr (lambda (b acc)
                                       (if (string=? "Delete" (cdr b))
                                           (cons (symbol->string (car b)) acc)
                                           acc))
                                     null
                                     update-bindings))]
               [new-configuration
                (cond
                  [(assq 'add-host update-bindings)
                   (add-virtual-host form-configuration (extract-bindings 'host-prefixes update-bindings))]
                  [(reverse-assoc edit-host-button-name update-bindings)
                   =>
                   (lambda (edit)
                     ; write the configuration twice when editing a host: once before and once after.
                     ; The after may never happen if the user doesn't continue
                     (write-configuration form-configuration configuration-path)
                     (configure-hosts form-configuration (string->number (symbol->string (car edit)))))]
                  [else form-configuration])])
          (write-configuration new-configuration configuration-path)
          (loop new-configuration))))))

; switch-to-current-port : configuration-table -> (U #f configuration-table)
; doesn't work - the browser doesn't send the port and it wouldn't be reliable anyway
; perhaps the server could include it?
#;(define (switch-to-current-port old)
    (let ([current-port (url-port (request-uri initial-request))])
      (and (not (= current-port (configuration-table-port old)))
           (make-configuration-table
            current-port
            (configuration-table-max-waiting old)
            (configuration-table-initial-connection-timeout old)
            (configuration-table-default-host old)
            (configuration-table-virtual-hosts old)))))

; send-exn : tst -> doesn't
(define (send-exn exn)
  (send/back (exception-error-page exn)))

; reverse-assoc : a (listof (cons b a)) -> (U #f (cons b a))
(define (reverse-assoc x lst)
  (cond
    [(null? lst) #f]
    [else (if (equal? x (cdar lst))
              (car lst)
              (reverse-assoc x (cdr lst)))]))

; add-virtual-host : configuration-table (listof str) -> configuration-table
(define (add-virtual-host conf existing-prefixes)
  (update-hosts conf (cons (cons "my-host.my-domain.org"
                                 (configuration-table-default-host conf))
                           (configuration-table-virtual-hosts conf))))

; update-hosts : configuration-table (listof (cons str host-table))
(define (update-hosts conf new-hosts)
  (make-configuration-table
   (configuration-table-port conf)
   (configuration-table-max-waiting conf)
   (configuration-table-initial-connection-timeout conf)
   (configuration-table-default-host conf)
   new-hosts))

; delete-hosts : configuration-table (listof str) -> configuration-table
; pre: (>= (length (configuration-table-virtual-hosts conf)) (max to-delete))
(define (delete-hosts conf to-delete)
  ; the if is not needed, it just avoids some work
  (if (null? to-delete)
      conf
      (update-hosts
       conf
       (drop (configuration-table-virtual-hosts conf) to-delete))))

; drop : (listof a) (listof str) -> (listof a)
; pre: (apply < to-delete)
; to delete the entries in to-filter indexed by to-delete
(define (drop to-filter to-delete)
  (let loop ([to-filter to-filter] [to-delete (map string->number to-delete)] [i 0])
    (cond
      [(null? to-delete) to-filter]
      [else (if (= i (car to-delete))
                (loop (cdr to-filter) (cdr to-delete) (add1 i))
                (cons (car to-filter) (loop (cdr to-filter) to-delete (add1 i))))])))

; configure-hosts : configuration-table (U #f nat) -> configuration-table
; n is either the virtual host number or #f for the default virtual host
(define (configure-hosts old n)
  (if n
      (update-hosts old
                    ; more here - consider restructuring this map.  Perhaps it is fine.
                    ; Perhaps it should short circuit.  Perhaps the number of virtual hosts
                    ; is small so it doesn't matter. Perhaps that is a sloppy way to think/program.
                    ; The code is really a functional array update except it's on a list.
                    (map (lambda (host this-n)
                           (if (= n this-n)
                               (cons (car host) (configure-host (cdr host)))
                               host))
                         (configuration-table-virtual-hosts old)
                         (build-list (length (configuration-table-virtual-hosts old)) (lambda (x) x))))
      (make-configuration-table
       (configuration-table-port old)
       (configuration-table-max-waiting old)
       (configuration-table-initial-connection-timeout old)
       (configure-host (configuration-table-default-host old))
       (configuration-table-virtual-hosts old))))

; configure-host : host-table -> host-table
(define (configure-host old)
  (let* ([bindings (interact (request-new-host-table old))]
         [new (update-host-table old bindings)])
    (when (assq 'edit-passwords bindings)
      (let* ([paths (host-table-paths new)]
             [password-path
              ;; build-path-unless-absolute is defined in configuration
              (build-path-unless-absolute
               (build-path-unless-absolute web-base (paths-host-base paths))
               (paths-passwords paths))])
        (unless (file-exists? password-path)
          (write-to-file password-path ''()))
        (configure-passwords password-path)))
    new))

(define restart-message
  `((h3 (font ([color "red"]) "Restart the Web server to use the new settings."))))

; request-new-configuration-table : configuration-table configuration-table -> str -> html
(define (request-new-configuration-table old orig)
  (build-suspender
   '("Racket Web Server Configuration")
   `((h1 "Racket Web Server Configuration Management")
     "copyright 2001 by Paul Graunke and PLT"
     (hr)
     (h2 "Basic Configuration")
     (table
      ,(make-tr-num "Port" 'port (configuration-table-port old))
      ,(make-tr-num "Maximum Waiting Connections"
                    'waiting (configuration-table-max-waiting old))
      ,(make-tr-num "Initial Connection Timeout (seconds)" 'time-initial
                    (configuration-table-initial-connection-timeout old)))
     (hr)
     (h2 "Host Name Configuration")
     (p "The Web server accepts requests on behalf of multiple " (em "hosts")
        " each corresponding to a domain name."
        " The table below maps domain names to host specific configurations.")
     (table ([width "50%"])
            (tr (th ([align "left"]) "Name") ;(th "Host configuration path")
                (th "Host Directory")
                (th nbsp)
                (th nbsp))
            (tr (td ,"Default Host")
                (td ,(make-field-size "text" 'default-host-root
                                      (table->host-root (configuration-table-default-host old))
                                      WIDE))
                (td ([align "center"])
                    (input ([type "submit"] [name "default"] [value ,edit-host-button-name])))
                (td nbsp))
            ,@(map (lambda (host n)
                     `(tr (td ,(make-field "text" 'host-regexps (car host)))
                          (td ,(make-field-size "text" 'host-roots (table->host-root (cdr host)) WIDE))
                          (td ([align "center"])
                              (input ([type "submit"] [name ,n] [value ,edit-host-button-name])))
                          (td ([align "center"])
                              (input ([type "submit"] [name ,n] [value "Delete"])))))
                   (configuration-table-virtual-hosts old)
                   (build-list (length (configuration-table-virtual-hosts old)) number->string))
            (tr (td (input ([type "submit"] [name "add-host"] [value "Add Host"])))
                (td nbsp); (input ([type "submit"] [name "configure"] [value "Delete"]))
                ;(td (input ([type "submit"] [name "edit-host-details"] [value "Edit"])))
                (td nbsp)))
     (hr)
     (table ([width "90%"])
            ,@(if (equal? old orig) ; This only tests eq? because structures are more opaque now.
                  null
                  `((tr (td ,@restart-message))))
            (tr (td (input ([type "submit"] [name "configure"] [value "Update Configuration"])))))
     (hr)
     ,footer)))

; table->host-root : host-table -> str
(define (table->host-root t)
  (path->string (build-path-unless-absolute web-base (paths-host-base (host-table-paths t)))))

; gen-make-tr : nat -> xexpr sym str [xexpr ...] -> xexpr
(define (gen-make-tr size-n)
  (let ([size-str (number->string size-n)])
    (lambda (label tag default-text . extra-tds)
      `(tr (td (a ([href ,(format "/~a/terms/~a.html" doc-dir tag)]) ,label))
           (td ,(make-field-size "text" tag (format "~a" default-text) size-str))
           . ,extra-tds))))

(define make-tr-num (gen-make-tr 20))

(define make-tr-str (gen-make-tr 70))

; make-field : str sym str -> xexpr
(define (make-field type label value)
  (make-field-size type label value "30"))

; make-field-size : str sym str str -> xexpr
(define (make-field-size type label value size)
  `(input ([type ,type] [name ,(symbol->string label)] [value ,value] [size ,size])))

; update-table : configuration-table bindings -> configuration-table
(define (update-table old bindings)
  (let ([ubp (un-build-path web-base)]) ;; web-base returned by directory-part is a path
    (make-configuration-table
     (string->nat (extract-binding/single 'port bindings))
     (string->nat (extract-binding/single 'waiting bindings))
     (string->num (extract-binding/single 'time-initial bindings))
     (update-host-root (configuration-table-default-host old)
                       (ubp (build-path (extract-binding/single 'default-host-root bindings))))
     (map (lambda (h root pattern)
            (cons pattern (update-host-root (cdr h) (ubp (build-path root)))))
          (configuration-table-virtual-hosts old)
          (extract-bindings 'host-roots bindings)
          (extract-bindings 'host-regexps bindings)))))

; update-host-root : host-table str -> host-table
(define (update-host-root host new-root)
  (host-table<-paths host (paths<-host-base (host-table-paths host) new-root)))

; host-table<-paths : host-table paths -> host-table
; more here - create these silly functions automatically from def-struct macro
(define (host-table<-paths host paths)
  (make-host-table
   (host-table-indices host)
   (host-table-log-format host)
   (host-table-messages host)
   (host-table-timeouts host)
   paths))

; paths<-host-base : paths str -> paths
; more here - create these silly functions automatically from def-struct macro
(define (paths<-host-base paths host-base)
  (make-paths (paths-conf paths)
              host-base
              (paths-log paths)
              (paths-htdocs paths)
              (paths-servlet paths)
              (paths-mime-types paths)
              (paths-passwords paths)))

; string->num : str -> nat
(define (string->num str)
  (or (string->number str) (error 'string->nat "~s is not a number" str)))

; string->nat : str -> nat
(define (string->nat str)
  (let ([n (string->number str)])
    (if (and n (integer? n) (exact? n) (>= n 0))
        n
        (error 'string->nat "~s is not exactly a natural number" str))))

; request-new-host-table : host-table -> str -> response
(define (request-new-host-table old)
  (let* ([timeouts (host-table-timeouts old)]
         [paths (host-table-paths old)]
         [m (host-table-messages old)]
         [host-root (build-path-unless-absolute web-base (paths-host-base paths))]
         [conf (build-path-unless-absolute host-root (paths-conf paths))])
    (build-suspender
     '("Configure Host")
     `((h1 "Racket Web Server Host configuration")
       (input ([type "submit"] [value "Save Configuration"]))
       (hr)
       (table
        (tr (th ([colspan "2"]) "Paths"))
        ,(make-tr-str "Log file"
                      'path-log (build-path-unless-absolute host-root (paths-log paths)))
        ,(make-tr-str "Web document root"
                      'path-htdocs (build-path-unless-absolute host-root (paths-htdocs paths)))
        ,(make-tr-str "Servlet root"
                      'path-servlet (build-path-unless-absolute host-root (paths-servlet paths)))
        ,(make-tr-str "MIME Types"
                      'path-mime-types (build-path-unless-absolute host-root (paths-mime-types paths)))
        ,(make-tr-str "Password File"
                      'path-password (build-path-unless-absolute host-root (paths-passwords paths)))
        (tr (td ([colspan "2"])
                ,(make-field "submit" 'edit-passwords "Edit Passwords")))
        (tr (td ([colspan "2"]) (hr)))
        (tr (th ([colspan "2"]) "Message Paths"))
        ,(make-tr-str "Servlet error" 'path-servlet-message
                      (build-path-unless-absolute conf (messages-servlet m)))
        ,(make-tr-str "Access Denied" 'path-access-message
                      (build-path-unless-absolute conf (messages-authentication m)))
        ,(make-tr-str "Servlet cache refreshed" 'servlet-refresh-message
                      (build-path-unless-absolute conf (messages-servlets-refreshed m)))
        ,(make-tr-str "Password cache refreshed" 'password-refresh-message
                      (build-path-unless-absolute conf (messages-passwords-refreshed m)))
        ,(make-tr-str "File not found" 'path-not-found-message
                      (build-path-unless-absolute conf (messages-file-not-found m)))
        ,(make-tr-str "Protocol error" 'path-protocol-message
                      (build-path-unless-absolute conf (messages-protocol m)))
        ,(make-tr-str "Collect garbage" 'path-collect-garbage-message
                      (build-path-unless-absolute conf (messages-collect-garbage m)))
        (tr (td ([colspan "2"]) (hr)))
        (tr (th ([colspan "2"]) "Timeout Seconds"))
        ,(make-tr-num "Default Servlet" 'time-default-servlet (timeouts-default-servlet timeouts))
        ,(make-tr-num "Password" 'time-password (timeouts-password timeouts))
        ,(make-tr-num "Servlet Connection" 'time-servlet-connection (timeouts-servlet-connection timeouts))
        ,(make-tr-num "per Byte When Transfering Files" 'time-file-per-byte (timeouts-file-per-byte timeouts))
        ,(make-tr-num "Base When Transfering Files" 'time-file-base (timeouts-file-base timeouts)))
       (hr)
       (input ([type "submit"] [value "Save Configuration"]))
       ,footer))))

; update-host-table : host-table (listof (cons sym str)) -> host-table
(define (update-host-table old bindings)
  (let* ([eb (lambda (tag) (build-path (extract-binding/single tag bindings)))]
         [paths (host-table-paths old)]
         [host-root (paths-host-base paths)]
         [expanded-host-root (build-path-unless-absolute web-base host-root)]
         [conf (build-path-unless-absolute expanded-host-root (paths-conf paths))]
         [ubp (un-build-path expanded-host-root)]
         [eb-host-root (lambda (tag) (ubp (eb tag)))]
         [ubp-conf (un-build-path conf)]
         [eb-conf (lambda (tag) (ubp-conf (eb tag)))])
    (make-host-table
     (host-table-indices old)
     (host-table-log-format old)
     (apply make-messages
            (map eb-conf '(path-servlet-message path-access-message servlet-refresh-message password-refresh-message path-not-found-message path-protocol-message path-collect-garbage-message)))
     (apply make-timeouts
            (map (lambda (tag) (string->number (extract-binding/single tag bindings)))
                 '(time-default-servlet time-password time-servlet-connection time-file-per-byte time-file-base)))
     (let ([old-paths (host-table-paths old)])
       (apply make-paths
              (paths-conf old-paths)
              ((un-build-path web-base)
               (build-path (paths-host-base old-paths)))
              (map eb-host-root '(path-log path-htdocs path-servlet path-mime-types path-password)))))))

; un-build-path : path -> path -> string
; (GregP) Theory: this should return a string not a path so that the result can be
;                 written to the configuration file.
(define (un-build-path possible-base)
  (let ([base-list (path->list possible-base)])
    (lambda (path)
      (let ([path-list (path->list path)])
        (cond
          [(suffix base-list path-list)
           => (lambda (x) (path->string (apply build-path x)))]
          [else
           (path->string path)])))))

; suffix : (listof a) (listof a) -> (U #f (listof a))
; to return the extra elements in b after removing all elements from a in order
(define (suffix a b)
  (cond
    [(null? a) (if (null? b) #f b)]
    [else (cond
            [(null? b) #f]
            [else (and (equal? (car a) (car b))
                       (suffix (cdr a) (cdr b)))])]))

; Password Configuration

; configure-passwords : path -> void
(define (configure-passwords password-path)
  (edit-passwords
   password-path
   (if (file-exists? password-path)
       (call-with-input-file password-path read-passwords)
       null)))

; edit-passwords : path passwords -> passwords
(define (edit-passwords which-one passwords)
  (let* ([bindings (interact (password-updates which-one passwords))]
         [to-deactivate (extract-bindings 'deactivate bindings)]
         [again
          (lambda (new-passwords)
            (write-to-file which-one (format-passwords new-passwords))
            (edit-passwords which-one new-passwords))])
    (cond
      [(assq 'edit bindings)
       => (lambda (edit)
            (again (drop (map (let ([to-edit (string->number (cdr edit))])
                                (lambda (r n)
                                  (if (= to-edit n)
                                      (edit-realm r)
                                      r)))
                              passwords
                              (build-list (length passwords) (lambda (x) x)))
                         to-deactivate)))]
      [(assq 'add bindings)
       (again (cons (make-realm "new realm" "" null)
                    (drop passwords to-deactivate)))]
      [else (drop passwords to-deactivate)])))

; password-updates : path passwords -> request
(define (password-updates which-one passwords)
  (let ([which-one (path->string which-one)])
    (build-suspender
     `("Updating Passwords for " ,which-one)
     `((h1 "Updating Passwords for ")
       (h3 ,which-one)
       (h2 "You may wish to " (font ([color "red"]) "backup") " this password file.")
       (p "Each authentication " (em "realm") " password protects URLs that match a pattern. "
          "Choose a realm to edit below:")
       (table
        (tr (th "Realm Name") (th "Delete") (th "Edit"))
        . ,(map (lambda (realm n)
                  `(tr (td ,(realm-name realm))
                       (td ,(make-field "checkbox" 'deactivate n))
                       (td ,(make-field "radio" 'edit n))))
                passwords
                (build-list (length passwords) number->string)))
       ,(make-field "submit" 'add "Add Realm")
       ,(make-field "submit" 'edit-button "Edit")
       ,footer))))

; edit-realm : realm -> realm
(define (edit-realm realm)
  (let* ([bindings (interact (realm-updates realm))]
         [new-name (extract-binding/single 'realm-name bindings)]
         [new-pattern (extract-binding/single 'realm-pattern bindings)]
         [new-allowed
          (drop (map (lambda (u p) (make-user-pass (string->symbol u) p))
                     (extract-bindings 'user bindings)
                     (extract-bindings 'pass bindings))
                (extract-bindings 'deactivate bindings))])
    ; more here - check something?  Everything is a string or symbol, though.
    (cond
      [(assq 'add-user bindings)
       (edit-realm (make-realm new-name new-pattern
                               (cons (make-user-pass 'ptg "Racket-is-cool!") new-allowed)))]
      [(assq 'update bindings)
       (make-realm new-name new-pattern new-allowed)]
      [else (error 'edit-realm "Didn't find either 'add-user or 'update in ~s" bindings)])))

; realm-updates : realm -> request
(define (realm-updates realm)
  (build-suspender
   `("Update Authentication Realm " ,(realm-name realm))
   `((h1 "Update Authentication Realm")
     (table
      ,(make-tr-str "Realm Name" 'realm-name (realm-name realm))
      ,(make-tr-str "Protected URL Path Pattern" 'realm-pattern (realm-pattern realm)))
     (hr)
     (table
      (tr (th "User Name") (th "Password") (th "Delete"))
      . ,(map (lambda (x n)
                `(tr (td ,(make-field "text" 'user (symbol->string (user-pass-user x))))
                     (td ,(make-field "text" 'pass (user-pass-pass x)))
                     (td ,(make-field "checkbox" 'deactivate n))))
              (realm-allowed realm)
              (build-list (length (realm-allowed realm)) number->string)))
     (input ([type "submit"] [name "add-user"] [value "Add User"]))
     (input ([type "submit"] [name "update"] [value "Update Realm"]))
     ,footer)))

; read-passwords : iport -> passwords
; only works if the file starts with (quote ...)
(define (read-passwords in)
  (let ([raw (read in)])
    (unless (and (pair? raw) (eq? 'quote (car raw))
                 (null? (cddr raw)))
      (error 'read-passwords "The password file must be quoted to use the configuration tool."))
    (map (lambda (raw-realm)
           ; more here - error checking
           (make-realm (car raw-realm)
                       (cadr raw-realm)
                       (map (lambda (x) (make-user-pass (car x) (cadr x)))
                            (cddr raw-realm))))
         (cadr raw))))

; format-passwords : passwords -> s-expr
(define (format-passwords passwords)
  (list 'quote
        (map (lambda (r)
               (list* (realm-name r)
                      (realm-pattern r)
                      (map (lambda (x)
                             (list (user-pass-user x) (user-pass-pass x)))
                           (realm-allowed r))))
             passwords)))

; Little Helpers

; initialization-error-page : request -> response
(define (initialization-error-page initial-request)
  (response/xexpr
   `(html (head (title "Web Server Configuration Program Invocation Error"))
          (body ([bgcolor "white"])
                (p "Please direct your browser directly to the "
                   (a ([href ,(url->string (request-uri initial-request))]) "configuration program,")
                   " not through another URL.")
                ,footer))))

; done-page : html
(define done-page
  (response/xexpr
   ; more-here - consider adding more useful information
   `(html (head (title "done"))
          (body ([bgcolor "white"])
                (h2 "Configuration Saved.")
                (p "Click your browser's back button to continue configuring the server.")
                ,footer))))

; exception-error-page : TST -> html
(define (exception-error-page exn)
  (response/xexpr
   `(html (head (title "Error"))
          (body ([bgcolor "white"])
                (p "Servlet exception: "
                   (pre ,(exn->string exn)))
                ,footer))))

(define must-select-host-page
  (response/xexpr
   `(html (head (title "Web Server Configuration Error"))
          (body ([bgcolor "white"])
                (p "Please select which host to edit before clicking the Edit button.")
                ,footer))))

; io

; read-configuration : path -> configuration-table
(define (read-configuration configuration-path)
  (sexpr->configuration-table (call-with-input-file configuration-path read)))

; write-configuration : configuration-table path -> void
; writes out the new configuration file and
; also copies the configure servlet to the default-host's servlet directory
(define (write-configuration new configuration-path)
  (ensure-configuration-servlet configuration-path (configuration-table-default-host new))
  (ensure-configuration-paths new)
  (write-configuration-table new configuration-path))

; ensure-configuration-servlet : path host-table -> void
(define (ensure-configuration-servlet configuration-path host)
  (let* ([paths (host-table-paths host)]
         [root (build-path-unless-absolute web-base
                                           (paths-host-base paths))]
         [servlets-path
          (build-path (build-path-unless-absolute root (paths-servlet paths)) "servlets")])
    (ensure-config-servlet configuration-path servlets-path)
    (let ([defaults (build-path "Defaults")])
      (ensure* (build-path default-web-root "htdocs")
               (build-path-unless-absolute root (paths-htdocs paths))
               defaults))))

; ensure-configuration-paths : configuration-table -> void
; to ensure that all the referenced config files exist for an entire configuration
(define (ensure-configuration-paths configuration)
  (ensure-host-configuration (configuration-table-default-host configuration))
  (for-each (lambda (x) (ensure-host-configuration (cdr x)))
            (configuration-table-virtual-hosts configuration)))

; ensure-host-configuration : host-table -> void
; to ensure that all the referenced config files exist for a virtual host
(define (ensure-host-configuration host)
  (let* ([paths (host-table-paths host)]
         [host-base (build-path-unless-absolute web-base (paths-host-base paths))]
         [conf (build-path-unless-absolute host-base (paths-conf paths))]
         [log (build-path-unless-absolute host-base (paths-log paths))])
    ; skip passwords since a missing file is an okay default
    (ensure-directory-shallow conf)
    (ensure-directory-shallow host-base)
    ;(ensure-file log ...) ; empty log file is okay
    (ensure-directory-shallow (build-path-unless-absolute host-base (paths-htdocs paths)))
    (ensure-directory-shallow (build-path-unless-absolute host-base (paths-servlet paths)))
    (let* ([messages (host-table-messages host)]
           ; more here maybe - check default config file instead? maybe not
           [from-conf (build-path default-web-root "conf")]
           [copy-conf
            (lambda (from to)
              (let ([to-path (build-path-unless-absolute conf to)])
                ; more here - check existence of from path
                (copy-file* (build-path from-conf from) to-path)))])
      (copy-conf "passwords-refresh.html" (messages-passwords-refreshed messages))
      (copy-conf "servlet-refresh.html" (messages-servlets-refreshed messages))
      (copy-conf "forbidden.html" (messages-authentication messages))
      (copy-conf "protocol-error.html" (messages-protocol messages))
      (copy-conf "not-found.html" (messages-file-not-found messages))
      (copy-conf "servlet-error.html" (messages-servlet messages))
      (copy-conf "collect-garbage.html" (messages-collect-garbage messages)))))

; ensure-file : path path path -> void
; to copy (build-path from name) to (build-path to name), creating directories as
; needed if the latter does not already exist.
(define (ensure-file from to name)
  (let ([to (simplify-path to)])
    (ensure-directory-shallow to)
    (let ([to-path (build-path to name)])
      (unless (file-exists? to-path)
        (copy-file (build-path from name) to-path)))))

; copy-file* : str str -> void
(define (copy-file* from-path to-path)
  (unless (file-exists? to-path)
    (let-values ([(to-path-base to-path-name must-be-dir?) (split-path to-path)])
      (ensure-directory-shallow to-path-base))
    (copy-file from-path to-path)))

; ensure* : path path path -> void
;; GregP: Don't know what the heck this does (thanks Paul)
;;        but the first two arguments are now paths.
(define (ensure* from to name)
  (ensure-directory-shallow to)
  (let ([p (build-path from name)])
    (cond
      [(directory-exists? p)
       (unless (regexp-match? #rx"^(?:[.](?:git.*|svn)|CVS)$" (path->string name)) ; yuck
         (let ([dest (build-path to name)])
           (ensure-directory-shallow dest)
           (for-each (lambda (x) (ensure* p dest x))
                     (directory-list p))))]
      [(file-exists? p)
       (ensure-file from to name)])))

; ensure-directory-shallow : path -> void
(define (ensure-directory-shallow to)
  (unless (directory-exists? to)
    ; race condition - someone else could make the directory
    (make-directory* to)))

; ensure-config-servlet : str path -> void
; to create, if necessary, a stub configuration servlet that includes the main configuration servlet
; at the desired location in a new web tree
(define (ensure-config-servlet configuration-path servlets-path)
  (ensure-directory-shallow servlets-path)
  (let ([file-path (build-path servlets-path CONFIGURE-SERVLET-NAME)])
    (unless (file-exists? file-path) ; more here - check that it's a well formed servlet?
      (call-with-output-file
          file-path
        (lambda (out)
          (pretty-write
           `(module ,CONFIGURE-SERVLET-NAME racket
              (require (lib ,CONFIGURE-SERVLET-NAME "web-server" "private"))
              (provide (all-from (lib ,CONFIGURE-SERVLET-NAME "web-server" "private")))
              (set-config-path! ,(path->string configuration-path)))
           out)
          (newline out))))))

; extract-definition : sym (listof s-expr) -> s-expr
; to return the rhs from (def name rhs) not (def (name . args) body)
(define (extract-definition name defs)
  (or (ormap (lambda (def)
               (and (pair? def) (eq? 'define (car def))
                    (pair? (cdr def)) (eq? name (cadr def))
                    (pair? (cddr def))
                    (caddr def)))
             defs)
      (error 'extract-definition "definition for ~a not found" name)))

; passwords = str (i.e. path to a file)

(define build-path-maybe-expression->file-name caddr)

; main
(define (start initial-request)
  (error-print-width 800) ; 10-ish lines
  (check-ip-address initial-request)
  (choose-configuration-file))
