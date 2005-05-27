(require-library "suite.ss" "server" "test")
(require-library "web-hammer.ss" "server" "test")

; more here - search for a port that works
(define port 8080)
(broken? port)
; calling broken? also starts a server in another underlying OS process on the same machine

(define run1
  (server-performance (string->url (format "http://127.0.0.1:~a/" port))
                      16 0 120))
