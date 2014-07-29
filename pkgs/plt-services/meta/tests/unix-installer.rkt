#lang racket
(require remote-shell/ssh
         remote-shell/vbox
         net/url
         racket/date
         file/zip)

(module test racket/base)

;; ----------------------------------------
;; Configuration (adjust as needed)

(define vbox-name "Ubuntu Server 14.04")
(define vbox-host "192.168.56.107")
(define vbox-user "racket")
(define vbox-snapshot "init")

(define snapshot-site "http://pre-release.racket-lang.org/installers/")

(define min-racket-installers
  (list "racket-minimal-6.0.900.900-x86_64-linux-ubuntu-precise.sh"))

(define racket-installers
  (list "racket-6.0.900.900-x86_64-linux-ubuntu-precise.sh"))

(define work-dir (find-system-path 'temp-dir))

;; ----------------------------------------
;; Get installers and "base.zip" from snapshot

(define (get f #:sub [sub ""])
  (unless (file-exists? (build-path work-dir f))
    (printf "Getting ~a\n" f)
    (call/input-url (string->url (string-append snapshot-site sub f))
                    get-pure-port
                    (lambda (i)
                      (call-with-output-file*
                       (build-path work-dir f)
                       #:exists 'truncate
                       (lambda (o)
                         (copy-port i o)))))))

(for-each get min-racket-installers)
(for-each get racket-installers)
(get #:sub "base/" "base.zip")

;; ----------------------------------------
;; Construct a simple package

(define sample-pkg-dir (build-path work-dir "sample"))
(delete-directory/files sample-pkg-dir #:must-exist? #f)
(make-directory* sample-pkg-dir)
(call-with-output-file* 
 (build-path sample-pkg-dir "info.rkt")
 (lambda (o)
   (displayln "#lang info" o)
   (write '(define collection "sample") o)
   (write '(define deps '("base")) o)))
(call-with-output-file* 
 (build-path sample-pkg-dir "main.rkt")
 (lambda (o)
   (displayln "#lang racket/base" o)
   (write "sample" o)))

(define sample-zip-path (build-path work-dir "sample.zip"))
(parameterize ([current-directory work-dir])
  (when (file-exists? "sample.zip") (delete-file "sample.zip"))
  (zip "sample.zip" "sample" #:utc-timestamps? #t))

;; ----------------------------------------
;; Construct a simple program

(define progy-path (build-path work-dir "progy.rkt"))
(call-with-output-file*
 progy-path
 #:exists 'truncate
 (lambda (o)
   (displayln "#lang racket/base" o)
   (write '(require sample) o)))

;; ----------------------------------------

(for* ([min? '(#t #f)]
       [f (in-list (if min?
                       min-racket-installers
                       racket-installers))]
       ;; Unix-style install?
       [unix-style? '(#f #t)]
       ;; Change path of "shared" to "mine-all-mine"?
       [mv-shared? (if unix-style? '(#t #f) '(#f))]
       ;; Install into "/usr/local"?
       [usr-local? '(#t #f)]
       ;; Link in-place install executables in "/usr/local/bin"?
       [links? (if unix-style? '(#f) '(#t #f))])
  (printf (~a "=================================================================\n"
              "CONFIGURATION: "
              (if min? "minimal" "full") " "
              (if unix-style? "unix-style" "in-place") " "
              (if mv-shared? "mine-all-mine " "")
              (if usr-local? "/usr/local " "")
              (if links? "linked" "")
              "\n"))

  (restore-vbox-snapshot vbox-name vbox-snapshot)

  (#%app
   dynamic-wind
   
   (lambda ()
     (start-vbox-vm vbox-name #:pause-seconds 0))
   
   (lambda ()
     (define rt (remote #:host vbox-host
                        #:user vbox-user))
     
     (make-sure-remote-is-ready rt)
     
     (ssh rt "sudo date --set=\""
          (parameterize ([date-display-format 'rfc2822])
            (date->string (seconds->date (current-seconds)) #t))
          "\"")
     
     (scp rt (build-path work-dir f) (at-remote rt f))

     (define script (build-path work-dir "script"))
     (call-with-output-file*
      script
      #:exists 'truncate
      (lambda (o)
        ;; Installer interactions:
        ;; 
        ;; Unix-style distribution?
        ;;  * yes -> 
        ;;     Where to install?
        ;;       [like below]
        ;; 
        ;;     Target directories
        ;;       [e]
        ;;       ...
        ;; 
        ;;  * no ->  
        ;;     Where to install?
        ;;       * 1 /usr/racket
        ;;       * 2 /usr/local/racket
        ;;       * 3 ~/racket
        ;;       * 4 ./racket
        ;;       * <anything else>
        ;; 
        ;;     Prefix for link?
        (fprintf o "~a\n" (if unix-style? "yes" "no"))
        (fprintf o (if usr-local?
                       "2\n"
                       "4\n"))
        (when mv-shared?
          (fprintf o "s\n") ; "shared" path
          (fprintf o "~a\n" (if usr-local?
                                "/usr/local/mine-all-mine"
                                "mine-all-mine")))
        (when links?
          (fprintf o "/usr/local\n"))
        (fprintf o "\n")))
     (scp rt script (at-remote rt "script"))

     (when min?
       (scp rt (build-path work-dir "base.zip") (at-remote rt "base.zip")))
     (scp rt sample-zip-path (at-remote rt "sample.zip"))
     (unless min?
       (scp rt progy-path (at-remote rt "progy.rkt")))

     (define sudo? (or usr-local? links?))
     (define sudo (if sudo? "sudo " ""))

     ;; install --------------------
     (ssh rt sudo "sh " f " < script")

     (define bin-dir
       (cond
        [(or links? (and usr-local? unix-style?)) ""]
        [else
         (~a (if usr-local?
                 "/usr/local/"
                 "")
             (if unix-style?
                 "bin/"
                 "racket/bin/"))]))

     ;; check that Racket runs --------------------
     (ssh rt (~a bin-dir "racket") " -e '(displayln \"hello\")'")

     ;; check that `raco setup` is ok --------------------
     ;;  For example, there are no file-permission problems.
     (ssh rt (~a bin-dir "raco") " setup" (if sudo?
                                              " --avoid-main"
                                              ""))

     ;; install and use a package --------------------
     (ssh rt (~a bin-dir "raco") " pkg install sample.zip" (if min? " base.zip" ""))
     (ssh rt (~a bin-dir "racket") " -l sample")

     ;; create a stand-alone executable ----------------------------------------
     (unless min?
       (ssh rt (~a bin-dir "raco") " exe progy.rkt")
       (ssh rt "./progy")
       (ssh rt (~a bin-dir "raco") " distribute d progy")
       (ssh rt "d/bin/progy"))

     ;; uninstall ----------------------------------------
     (when unix-style?
       (ssh rt sudo (~a bin-dir "racket-uninstall"))
       (when (ssh rt (~a bin-dir "racket") #:mode 'result)
         (error "not uninstalled")))

     ;; check stand-alone executable ----------------------------------------
     (unless min?
       (ssh rt "d/bin/progy"))
     
     (void))

   (lambda ()
     (stop-vbox-vm vbox-name))))
