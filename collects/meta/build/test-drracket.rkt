#!/bin/sh
#| -*- scheme -*-
exec "$PLTHOME/bin/gracket" "$0"
|#

#lang racket/gui

;; save the original error port to send messages
(define stderr (current-error-port))
(define (die fmt . args)
  (apply fprintf stderr fmt args)
  (newline stderr)
  (exit 1))

(define (cleanup)
  (when (directory-exists? (find-system-path 'pref-dir))
    (delete-directory/files (find-system-path 'pref-dir))))

(define (my-handler e)
  (cleanup)
  (die "uncaught exception: ~a\n" (if (exn? e) (exn-message e) e)))

(define-values (in out) (make-pipe))
((compose void thread)
 (lambda ()
   (let* ([bytes (make-bytes 1000)]
          [len/eof (sync (read-bytes-avail!-evt bytes in))])
     (die "text printed to stdout/stderr:\n~a\n"
          (if (eof-object? len/eof) len/eof (subbytes bytes 0 len/eof))))))

(uncaught-exception-handler my-handler)
(current-output-port out)
(current-error-port out)

;; must create eventspace after setting parameters, so its thread
;; inherits the new settings
(define es (make-eventspace))

(current-eventspace es)
(void (thread (lambda () (sleep 120) (die "timeout!"))))

;; make sure the preferences are such that we don't get the welcome screen
(cleanup)
(make-directory (find-system-path 'pref-dir))
(with-output-to-file (find-system-path 'pref-file) #:exists 'truncate
  (lambda ()
    (printf "~s\n" `((plt:framework-prefs
                      ((drracket:last-version ,(version))
                       (drracket:last-language english)))))))

;; start drracket, get interface for testing its windows
(define <%> #f)
(queue-callback (lambda ()
                  (dynamic-require 'drracket #f)
                  (set! <%> (dynamic-require 'drracket/tool-lib
                                             'drracket:unit:frame<%>))))

(define (is-drracket-frame? win) (and <%> (is-a? win <%>)))

;; wait for the drracket window to appear
(define (window-title w) (send w get-label))
(let loop ()
  (sleep 1/100)
  (let ([wins (get-top-level-windows)])
    (cond
      ;; wait to have windows
      [(null? wins) (loop)]
      ;; that are all drracket frames
      [(not (andmap is-drracket-frame? wins)) (loop)]
      [(pair? (cdr wins))
       (die "too many windows popped up: ~s" (map window-title wins))]
      [(regexp-match #rx"^Untitled( - DrRacket)?$" (window-title (car wins)))
       (fprintf stderr "got a good window: ~a\n" (window-title (car wins)))]
      [else (die "bad window popped up: ~s" (window-title (car wins)))])))

;; handle some events
(let loop ([n 20]) (unless (zero? n) (yield) (loop (sub1 n))))

;; queue a low priority callback to exit sucessfully
(queue-callback (lambda () (cleanup) (exit 0)) #f)
