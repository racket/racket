(module help-desk-server mzscheme

;; PURPOSE
;;   This file launches a web-server serving an online
;;   version of the HelpDesk pages.
;;   This is intended for testing the online version,
;;   not as a way of deplying it.

;; NOTES
;;   The web-server uses the port given by internal-port
;;   in "private/options.ss" by default.

;; Startpage:
;;   http://localhost:8012/servlets/home.ss
;;   (where 8012 is the port given by internal-port)


(require (lib "web-server.ss" "web-server")
         (lib "web-config-unit.ss" "web-server")
         "private/config.ss"
         "private/internal-hp.ss"
         "private/options.ss"
         (lib "cmdline.ss"))

(helpdesk-platform 'external-browser)

(command-line
 "help-desk-server"
 (current-command-line-arguments)
 (once-each
  [("-p" "--port") port "port to run on"
   (internal-port (string->number port))]))

;; start the HelpDesk server, and store a shutdown
(define shutdown
  (serve/web-config@ (make-config)))

(printf "\nStart here: http://~a:~a/servlets/home.ss\n\n"
        internal-host (internal-port))

(printf "Press enter to shutdown.\n")
(with-handlers ([exn:break? (lambda (exn) (shutdown) (exit))])
  (read-line))
(shutdown)

)
