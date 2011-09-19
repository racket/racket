#lang racket/base
(require racket/file racket/port net/url setup/plt-installer racket/gui/base
         framework "info.rkt" "this-collection.rkt")

(define name        (#%info-lookup 'name))
(define web-address (#%info-lookup 'web-address))
(define version-filename (#%info-lookup 'version-filename))
(define package-filename (#%info-lookup 'package-filename))
(define dialog-title (string-append name " Updater"))
(define (file->inport filename)
  (get-pure-port
   (string->url
    (string-append (regexp-replace #rx"/?$" web-address "/") filename))))
(define update-key (make-my-key 'update-check))
(preferences:set-default update-key #t boolean?)

(define (update!)
  (let* ([in   (file->inport package-filename)]
         [outf (make-temporary-file "tmp~a.plt")]
         [out  (open-output-file outf #:mode 'binary #:exists 'truncate)])
    (dynamic-wind
      void
      (lambda () (copy-port in out))
      (lambda () (close-input-port in) (close-output-port out)))
    (run-installer outf (lambda () (delete-file outf)))))
(define (maybe-update parent new-version)
  (define response
    (message-box/custom
     dialog-title
     (string-append
      "A new version of the "name" plugin is available: "
      (let ([v (format "~a" new-version)])
        (if (= 12 (string-length v))
          (apply format "~a~a~a~a/~a~a/~a~a ~a~a:~a~a" (string->list v))
          v)))
     "&Update now" "Remind Me &Later"
     ;; may be disabled, but explicitly invoked through menu item
     (if (preferences:get update-key)
       "&Stop Checking" "Update and &Always Check")
     parent '(default=1 caution) 2))
  (case response
    [(1) (update!)]
    [(2) 'ok] ; do nothing
    [(3) (preferences:set update-key (not (preferences:get update-key)))
     (when (preferences:get update-key) (update!))]
    [else (error 'update "internal error in ~a plugin updater" name)]))
(provide update)
(define (update parent . show-ok?)
  (let* ([web-version
          (with-handlers ([void (lambda _ 0)])
            (let ([in (file->inport version-filename)])
              (dynamic-wind
                void
                (lambda () (read in))
                (lambda () (close-input-port in)))))]
         ;; if the file was not there, we might have read some junk
         [web-version (if (integer? web-version) web-version 0)]
         [current-version
          (with-input-from-file (in-this-collection "version") read)])
    (cond [(> web-version current-version) (maybe-update parent web-version)]
          [(and (pair? show-ok?) (car show-ok?))
           (message-box dialog-title "Your plugin is up-to-date" parent)])))

(define (wait-for-top-level-windows)
  ;; wait until the definitions are instantiated, return top-level window
  (let ([ws (get-top-level-windows)])
    (if (null? ws) (begin (sleep 1) (wait-for-top-level-windows)) (car ws))))
(provide bg-update)
(define (bg-update)
  (thread (lambda ()
            (when (preferences:get update-key)
              (update (wait-for-top-level-windows))))))
