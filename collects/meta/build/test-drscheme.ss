#!/bin/sh
#| -*- scheme -*-
exec "$PLTHOME/bin/mred" "$0"
|#

#lang mzscheme

(require (lib "mred.ss" "mred") (lib "class.ss") (lib "port.ss") (lib "file.ss"))

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
(thread
 (lambda ()
   (let* ([bytes (make-bytes 1000)]
          [len/eof (sync (read-bytes-avail!-evt bytes in))])
     (die "got some data printed to stdout/stderr:\n~a\n"
          (if (eof-object? len/eof) len/eof (subbytes bytes 0 len/eof))))))

(uncaught-exception-handler my-handler)
(current-output-port out)
(current-error-port out)

;; must create eventspace after setting parameters, so its thread
;; inherits the new settings
(define es (make-eventspace))

(current-eventspace es)
(thread (lambda () (sleep 60) (die "timeout!")))

;; make sure the preferences are such that we don't get the welcome screen
(cleanup)
(make-directory (find-system-path 'pref-dir))
(with-output-to-file (find-system-path 'pref-file)
  (lambda ()
    (printf "~s\n" `((plt:framework-prefs
                      ((drscheme:last-version ,(version))
                       (drscheme:last-language english))))))
  'truncate)

;; start drscheme
(queue-callback
 (lambda ()
   (dynamic-require '(lib "drscheme.ss" "drscheme") #f)))

;; wait for the drscheme window to appear
(define (window-title w) (send w get-label))
(let loop ()
  (sleep 1/100)
  (let ([wins (get-top-level-windows)])
    (cond [(null? wins) (loop)]
          [(and (regexp-match #rx"^Untitled( - DrScheme)?$"
                              (window-title (car wins)))
                (null? (cdr wins)))
           (fprintf stderr "got a good window: ~a\n"
                    (window-title (car wins)))]
          [else (die "bad windows popped up: ~s" (map window-title wins))])))

;; handle some events
(let loop ([n 20]) (unless (zero? n) (yield) (loop (sub1 n))))

;; queue a low priority callback to exit sucessfully
(queue-callback (lambda () (cleanup) (exit 0)) #f)
