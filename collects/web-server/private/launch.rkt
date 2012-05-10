#lang racket/base
(require racket/cmdline
         racket/unit
         net/tcp-sig
         net/tcp-unit
         net/ssl-tcp-unit)
(require web-server/configuration/configuration-table
         (except-in web-server/web-server serve)
         web-server/web-config-unit)

; this is used by launchers
; extract-flag : sym (listof (cons sym alpha)) alpha -> alpha
(define (extract-flag name flags default)
  (let ([x (assq name flags)])
    (if x
        (cdr x)
        default)))

(define ssl (make-parameter #f))
(define port (make-parameter #f))

(define configuration@
  (parse-command-line
   "plt-web-server"
   (current-command-line-arguments)
   `((once-each
      [("--ssl")
       ,(lambda (flag)
          (unless (port) (port 443))
          (ssl #t))
       ("Run with SSL using server-cert.pem and private-key.pem in the current directory, with 443 as the default port.")]
      [("-f" "--configuration-table")
       ,(lambda (flag file-name)
          (cond
            [(not (file-exists? file-name))
             (error 'web-server "configuration file ~s not found" file-name)]
            [(not (memq 'read (file-or-directory-permissions file-name)))
             (error 'web-server "configuration file ~s is not readable" file-name)]
            [else (cons 'config (string->path file-name))]))
       ("Use an alternate configuration table" "file-name")]
      [("-p" "--port")
       ,(lambda (flag the-port)
          (let ([p (string->number the-port)])
            (if (and (integer? p) (<= 1 p 65535))
              (port p)
              (error 'web-server "expecting a valid port number, got \"~a\"" the-port))))
       ("Use an alternate network port." "port")]
      [("-a" "--ip-address")
       ,(lambda (flag ip-address)
          ; note the double backslash I initially left out.  That's a good reason to use Olin's regexps.
          (let ([addr (regexp-split #px"\\." ip-address)])
            (if (and (= 4 (length addr))
                     (andmap (lambda (s)
                               (let ([n (string->number s)])
                                 (and (integer? n) (<= 0 n 255))))
                             addr))
                (cons 'ip-address ip-address)
                (error 'web-server "ip-address expects a numeric ip-address (i.e. 127.0.0.1); given ~s" ip-address))))
       ("Restrict access to come from ip-address" "ip-address")]))
   (lambda (flags)
     (configuration-table->web-config@
      (extract-flag 'config flags default-configuration-table-path)
      #:port (port)
      #:listen-ip (extract-flag 'ip-address flags #f)))
   '()))

(define (serve)
  (serve/web-config@ 
   configuration@
   #:tcp@ (if (ssl)
              (let ()
                (define-unit-binding ssl-tcp@
                  (make-ssl-tcp@ (build-path (current-directory) "server-cert.pem")
                                 (build-path (current-directory) "private-key.pem")
                                 #f #f #f #f #f)
                  (import) (export tcp^))
                ssl-tcp@)
              tcp@)))

(provide serve)
