; The main program of the "web-server" launcher.
(module launch mzscheme
  (require (lib "cmdline.ss")
           (lib "pregexp.ss")
           "util.ss"
           "web-server-unit.ss"
           "sig.ss"
           "configuration.ss"
           (lib "unitsig.ss")
           (lib "tcp-sig.ss" "net"))

  (define configuration@
    (parse-command-line
     "web-server"
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
            (let ([p (string->number port)])
              (if (and (number? p) (integer? p) (exact? p) (<= 1 p 65535))
                  (cons 'port p)
                  (error 'web-server "port expects an argument of type <exact integer in [1, 65535]>; given ~s" port))))
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
       (update-configuration
        (load-configuration
         (extract-flag 'config flags default-configuration-table-path))
        flags))
     '()))

  (define-values/invoke-unit/sig web-server^
    (compound-unit/sig
     (import (T : net:tcp^))
     (link
      [C : web-config^ (configuration@)]
      [S : web-server^ (web-server@ T C)])
     (export (open S)))
    #f net:tcp^)

  (provide serve))
