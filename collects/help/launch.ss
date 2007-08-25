;;; launch.ss

; PURPOSE
;   This file launches a web-server serving an online
;   version of the HelpDesk pages.
;   This is intended for testing the online version,
;   not as a way of deplying it.

; NOTES
;   The web-server uses the port given by internal-port 
;   in "collects/help/private/internal-hp.ss".

;   Change the parameter current-helpdesk-platform
;   in "collects/help/servlets/private/platform.ss"
;   to 'external-browser when testing the online version.

;  Startpage:
;     http://localhost:8000/servlets/home.ss
;     (where 8000 is the port given by internal-port)

(require (lib "web-server.ss" "web-server")
         (lib "web-config-unit.ss" "web-server")
         "private/config.ss"
         "private/internal-hp.ss")

; start the HelpDesk server, and store a shutdown 
(define shutdown 
  (serve/web-config@ config))

(display "Did you remember to change current-helpdesk-platform in platform.ss?\n\n")
(display (format "Start here: http://~a:~a/servlets/home.ss\n\n"
                 internal-host internal-port))

(display "Press enter to shutdown.\n")
(read-line)
;(shutdown)
