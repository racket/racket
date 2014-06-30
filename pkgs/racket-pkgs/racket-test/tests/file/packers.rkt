#lang racket/base
(require file/tar file/zip
         file/untar file/unzip
         racket/file racket/system racket/set
         tests/eli-tester)

(define (make-file path)
  (with-output-to-file path
    (lambda ()
      (for ([i (in-range (random 1000))])
        (write-bytes (make-bytes (random 100) (+ 32 (random 96))))))))

(define (file-or-directory-permissions* path permissions)
  (file-or-directory-permissions
   path
   (for/fold ([n 0]) ([p '(["r" #o400] ["w" #o200] ["x" #o100])])
     (if (regexp-match? (car p) permissions) (bitwise-ior n (cadr p)) n))))

(define (diff src dest check-attributes?)
  (define (compare-attributes p1 p2)
    (or (not check-attributes?)
        (and (or (and (eq? check-attributes? 'file)
                      (directory-exists? p1))
                 (= (round-date (file-or-directory-modify-seconds p1))
                    (round-date (file-or-directory-modify-seconds p2)))
                 (begin
                   (printf "~s ~s ~s\n"
                           p1 
                           (file-or-directory-modify-seconds p1)
                           (file-or-directory-modify-seconds p2))
                   #f))
             (equal? (file-or-directory-permissions p1)
                     (file-or-directory-permissions p2)))))
  (define (round-date s)
    (if (eq? check-attributes? 'file)
        ;; granularity of ".zip" file dates is 2 seconds(!)
        (if (even? s) s (add1 s)) ; round to future is the default
        s))
  (cond
    [(link-exists? src)
     (and (link-exists? dest)
          (diff (resolve-path src) (resolve-path dest) check-attributes?))]
    [(file-exists? src)
     (and (file-exists? dest)
          (= (file-size src) (file-size dest))
          (compare-attributes src dest)
          (equal? (file->bytes src) (file->bytes dest)))]
    [(directory-exists? src)
     (and (directory-exists? dest)
          (compare-attributes src dest)
          (let* ([sort-paths (λ (l) (sort l bytes<? #:key path->bytes))]
                 [srcs       (sort-paths (directory-list src))]
                 [dests      (sort-paths (directory-list dest))])
            (and (equal? srcs dests)
                 (for/and ([src-item (in-list srcs)]
                           [dest-item (in-list dests)])
                   (diff (build-path src src-item)
                         (build-path dest dest-item)
                         check-attributes?))
                 ;; make dest writable to simplify clean-up:
                 (begin (file-or-directory-permissions* dest "rwx") #t))))]
    [else #t]))

(define (zip-tests zip unzip timestamps?
                   #:dir-name [ex1 "ex1"]
                   #:file-name [f2 "f2"])
  (make-directory* ex1)
  (make-file (build-path ex1 "f1"))
  (make-file (build-path ex1 f2))
  (make-file (build-path ex1 "f3"))
  (define more-dir (build-path ex1 "more"))
  (make-directory* more-dir)
  (make-file (build-path more-dir "f4"))

  (zip "a.zip" ex1)
  (when timestamps? (sleep 3)) ; at least 2 seconds, plus 1 to likely change parity

  (make-directory* "sub")
  (parameterize ([current-directory "sub"])
    (unzip "../a.zip"))
  
  (unless (diff ex1 (build-path "sub" ex1) timestamps?)
    (eprintf "changed! ~s\n" zip))

  (delete-directory/files "sub")
  (delete-file "a.zip")

  (zip "a.zip" #:path-prefix "inside" ex1)
  (make-directory* "sub")
  (parameterize ([current-directory "sub"])
    (unzip "../a.zip"))

  (unless (diff ex1 (build-path "sub" "inside" ex1) timestamps?)
    (eprintf "changed! ~s\n" zip))
  
  (delete-file "a.zip")
  (delete-directory/files "sub")
  (delete-directory/files ex1))

(define work-dir (make-temporary-file "packer~a" 'directory))

(define (make-zip utc?)
  (lambda (#:path-prefix [prefix #f] . args)
    (apply zip #:path-prefix prefix args #:utc-timestamps? utc?)))

(define (make-unzip utc?)
  (lambda args
    (apply unzip #:preserve-timestamps? #t #:utc-timestamps? utc? args)))

(parameterize ([current-directory work-dir])
  (zip-tests zip unzip #f)
  (zip-tests (make-zip #f) (make-unzip #f) 'file)
  (zip-tests (make-zip #t) (make-unzip #t) 'file)
  (zip-tests tar untar #t)
  (zip-tests tar untar #t
             #:dir-name (make-string 64 #\d)
             #:file-name (make-string 64 #\f)))

(delete-directory/files work-dir)

'ok
