;;; launch.ss

;; PURPOSE
;;   This file launches a web-server serving an online
;;   version of the HelpDesk pages.
;;   This is intended for testing the online version,
;;   not as a way of deplying it.

;; NOTES
;;   The web-server uses the port given by internal-port
;;   in "collects/help/private/internal-hp.ss".

;;  Startpage:
;;     http://localhost:8000/servlets/home.ss
;;     (where 8000 is the port given by internal-port)

(require (lib "web-server.ss" "web-server")
         (lib "web-config-unit.ss" "web-server")
         "private/config.ss"
         (only "private/internal-hp.ss" internal-host)
         "private/options.ss")

(helpdesk-platform 'external-browser)

;; start the HelpDesk server, and store a shutdown
(define shutdown
  (serve/web-config@ config))

(printf "\nStart here: http://~a:~a/servlets/home.ss\n\n"
        internal-host (internal-port))

(printf "Press enter to shutdown.\n")
(read-line)
;(shutdown)
