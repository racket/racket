#lang racket/base
(require racket/file
         file/zip
         file/unzip)

;; checks that timestamps and file permissions
;; are zipped and unzipped correctly

(define dir (make-temporary-directory))
(define zip (make-temporary-file "~a.zip"))

(call-with-output-file (build-path dir "x")
  (lambda (o) (fprintf o "123\n")))
(call-with-output-file (build-path dir "y")
  (lambda (o) (fprintf o "abc\n")))
(make-directory (build-path dir "d"))
(call-with-output-file (build-path dir "d" "z")
  (lambda (o) (fprintf o "xyz\n")))

(file-or-directory-permissions (build-path dir "d" "z")
                               (if (eq? 'windows (system-type))
                                   #o555
                                   #o444))

(call-with-output-file zip
  #:exists 'truncate
  (lambda (out)
    (parameterize ([current-directory dir])
      (zip->output (list "x" "y" "d" "d/z") out))))

;; ".zip" dates are even numbers
(define (rounded v)
  (+ v (modulo v 2)))

(define (test unzip)
  (define dir2 (make-temporary-directory))
  (parameterize ([current-directory dir2])
    (unzip zip #:preserve-attributes? #t))

  (define (check-same a b)
    (unless (equal? a b)
      (error "different" a b)))

  (define (compare f)
    (check-same (file->string (build-path dir f))
                (file->string (build-path dir2 f)))
    (check-same (rounded (file-or-directory-modify-seconds (build-path dir f)))
                (rounded (file-or-directory-modify-seconds (build-path dir2 f))))
    (check-same (file-or-directory-permissions (build-path dir f))
                (file-or-directory-permissions (build-path dir2 f))))

  (compare "x")
  (compare "y")
  (compare (build-path "d" "z"))

  (unless (eq? 'windows (system-type))
    (check-same (rounded (file-or-directory-modify-seconds (build-path dir "d")))
                (rounded (file-or-directory-modify-seconds (build-path dir2 "d")))))

  (delete-directory/files dir2))

"waiting 2 seconds"
(sleep 2)

(test unzip)
(test (lambda (path #:preserve-attributes? yes)
        (call-with-input-file
         path
         (lambda (in)
           (define zd (read-zip-directory in))
           (define todos
             (for/list ([e (in-list (zip-directory-entries zd))])
               (unzip-entry in zd e #:preserve-attributes? #t)))
           (for ([todo (in-list todos)])
             (when todo (todo)))))))

(delete-file zip)
(delete-directory/files dir)
