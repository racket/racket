#lang racket/base

(current-directory (build-path 'up))

(define app-path (vector-ref (current-command-line-arguments) 0))
(define old-name (vector-ref (current-command-line-arguments) 1))
(define new-name (vector-ref (current-command-line-arguments) 2))

(rename-file-or-directory
 (build-path app-path "Contents" "MacOS" old-name)
 (build-path app-path "Contents" "MacOS" "other"))

(rename-file-or-directory
 (build-path app-path "Contents" "MacOS" "other")
 (build-path app-path "Contents" "MacOS" new-name))

(rename-file-or-directory
 (build-path app-path "Contents" "Resources" (format "~a.icns" old-name))
 (build-path app-path "Contents" "Resources" "other"))
(rename-file-or-directory
 (build-path app-path "Contents" "Resources" "other")
 (build-path app-path "Contents" "Resources" (format "~a.icns" new-name)))

(let ([s (with-input-from-file (build-path app-path "Contents" "Info.plist")
           (lambda () (read-bytes 10000000)))])
  (with-output-to-file (build-path app-path "Contents" "Info.plist")
    #:exists 'truncate
    (lambda ()
      (printf "~a\n"
              (regexp-replace* (format "<string>~a</string>" old-name)
                               s
                               (format "<string>~a</string>" new-name))))))

