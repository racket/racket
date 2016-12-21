#lang racket/base
(require racket/file
         file/tar
         file/untar
         racket/system)

;; Paths and link targets longer than 100 to 255 characters are
;; trouble for tar. Check the extensions that handle those kinds
;; of paths.

(define tmp (make-temporary-file "tar~a" 'directory))

(define src-dir (build-path tmp "src"))
(define dest-dir (build-path tmp "dest"))

(define tar-bin (find-executable-path "tar"))

(define (check what . paths)
  (for ([format '(pax gnu exe)]
        #:when (or (not (eq? format 'exe)) tar-bin))
    (printf "Trying ~a ~a\n" what format)
    
    (delete-directory/files src-dir #:must-exist? #f)
    (delete-directory/files dest-dir #:must-exist? #f)
    
    (make-directory src-dir)
    (make-directory dest-dir)
    
    (for ([p (in-list paths)])
      (define link?
        (and (pair? p)
             (eq? 'link (car p))))
      (define-values (base name dir?)
        (split-path (if link? (cadr p) p)))
      (parameterize ([current-directory src-dir])
        (when (path? base) (make-directory* base))
        (if link?
            (make-file-or-directory-link (caddr p) (cadr p))
            (call-with-output-file
             p
             (lambda (o)
               (display (random) o))))))
    
    (parameterize ([current-directory src-dir])
      (case format
        [(exe)
         ;; `tar` may complain about weird paths, so redirect those
         ;; complaints to stdout to avoid a test failure:
         (parameterize ([current-error-port (current-output-port)])
           (apply system*
                  tar-bin
                  "cf"
                  "content.tar"
                  (for/list ([p (in-list paths)])
                    (if (pair? p) (cadr p) p))))]
        [else
         (apply tar
                "content.tar"
                #:format format
                (for/list ([p (in-list paths)])
                  (if (pair? p) (cadr p) p)))]))

    (parameterize ([current-directory dest-dir])
      (untar (build-path src-dir "content.tar")))
    
    (for/list ([p (in-list paths)])
      (define n (if (pair? p) (cadr p) p))
      (check-same (build-path src-dir n)
                  (build-path dest-dir n)))))

(define (check-same p1 p2)
  (cond
   [(link-exists? p1)
    (unless (link-exists? p2) (error 'tar-long-paths "not a link: ~s" p2))
    (unless (equal? (resolve-path p1) (resolve-path p2))
      (error 'tar-long-paths "links differ: ~s and ~s" p1 p2))]
   [else
    (unless (file-exists? p2) (error 'tar-long-paths "not unpacked: ~s" p2))
    (when (link-exists? p2) (error 'tar-long-paths "unpacked as link: ~s" p2))
    (unless (equal? (file->bytes p1) (file->bytes p2))
      (error 'tar-long-paths "files differ: ~s and ~s" p1 p2))]))

(check "one long"
       "one"
       "two"
       (string-append "three-" (make-string 100 #\x))
       "four")

(check "two long"
       "one"
       (string-append "sub/two-" (make-string 93 #\x))
       (string-append "sub/three-" (make-string 100 #\x))
       "four")

(unless (eq? 'windows (system-type))
  (check "encoding"
         (bytes->path #"one\xF0")
         "two\u3BB"
         (bytes->path (bytes-append #"sub/three\xF1-" (make-bytes 93 (char->integer #\x))))
         (string-append "sub/four\u3BB-" (make-string 93 #\x)))
  
  (check "long link"
         (string-append "one-" (make-string 150 #\x))
         `[link ,"two" ,(string-append "one-" (make-string 150 #\x))])
  
  (check "long link as long"
         (string-append "one-" (make-string 150 #\x))
         `[link ,(string-append "two-" (make-string 100 #\x)) ,(string-append "one-" (make-string 150 #\x))]))

(delete-directory/files tmp)
