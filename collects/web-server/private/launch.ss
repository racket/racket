#lang scheme/base
(require mzlib/cmdline
         mzlib/pregexp)
(require "../configuration/configuration-table.ss"
         (except-in "../web-server.ss" serve)
         "../web-config-unit.ss")

; this is used by launchers
; extract-flag : sym (listof (cons sym alpha)) alpha -> alpha
(define (extract-flag name flags default)
  (let ([x (assq name flags)])
    (if x
        (cdr x)
        default)))

(define configuration@
  (parse-command-line
   "plt-web-server"
   (current-command-line-arguments)
   `((once-each
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
       ,(lambda (flag port)
          (cons 'port (string->number port)))
       ("Use an alternate network port." "port")]
      [("-a" "--ip-address")
       ,(lambda (flag ip-address)
          ; note the double backslash I initially left out.  That's a good reason to use Olin's regexps.
          (let ([addr (pregexp-split "\\." ip-address)])
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
      #:port (extract-flag 'port flags #f)
      #:listen-ip (extract-flag 'ip-address flags #f)))
   '()))

(define (serve)
  (serve/web-config@ configuration@))

(provide serve)
