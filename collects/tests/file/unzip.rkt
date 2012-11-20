#lang racket/base
(require file/unzip
         racket/file
         racket/system)

(define tmp (find-system-path 'temp-dir))
(define zip-exe (find-executable-path "zip"))

(define work-dir (build-path tmp (format "unzip-testing~a" (random 1000))))
(printf "Working in ~a\n" work-dir)
(when (directory-exists? work-dir)
  (delete-directory/files work-dir))
(define a.zip (build-path work-dir "a.zip"))

(define sub-dir (build-path work-dir "sub"))

(define (make-file path)
  (with-output-to-file path
    (lambda ()
      (write-bytes (make-bytes (random 100000) 42))))
  (void))

(define ex1-dir (build-path work-dir "ex1"))
(define more-dir (build-path ex1-dir "more"))

(make-directory* ex1-dir)
(make-file (build-path ex1-dir "f1"))
(make-file (build-path ex1-dir "f2"))
(make-file (build-path ex1-dir "f3"))
(make-directory* more-dir)
(make-file (build-path more-dir "f4"))

(define (zip dir)
  (define-values (base name dir?) (split-path dir))
  (parameterize ([current-directory base])
    (void (system* zip-exe "-r" a.zip name))))

(define (diff-error src dest)
  (error 'diff "different: ~e ~e\n" src dest))

(define (diff src dest)
  (cond
   [(link-exists? src)
    (unless (link-exists? dest) (diff-error src dest))
    (diff (resolve-path src) (resolve-path dest))]
   [(file-exists? src)
    (unless (and (file-exists? dest) 
                 (= (file-size src) (file-size dest))
                 (equal? (file->bytes src) (file->bytes dest)))
      (diff-error src dest))]
   [(directory-exists? src)
    (unless (directory-exists? dest)
      (diff-error src dest))
    (define (sort-paths l)
      (sort l bytes<? #:key path->bytes))
    (define srcs (sort-paths (directory-list src)))
    (define dests (sort-paths (directory-list dest)))
    (unless (equal? srcs dests) (diff-error src dest))
    (for ([src-item (in-list srcs)]
          [dest-item (in-list dests)])
      (diff (build-path src src-item) (build-path dest dest-item)))]
   [else (void)]))

(zip ex1-dir)

(make-directory* sub-dir)
(parameterize ([current-directory sub-dir])
  (unzip a.zip))
(diff ex1-dir (build-path sub-dir "ex1"))
(delete-directory/files sub-dir)

(parameterize ([current-directory work-dir])
  (unzip a.zip (make-filesystem-entry-reader #:dest "sub")))
(diff ex1-dir (build-path sub-dir "ex1"))
(delete-directory/files sub-dir)

(parameterize ([current-directory work-dir])
  (unzip a.zip (lambda (bytes dir? in) (void))))
(when (directory-exists? sub-dir)
  (error "should not have been unpacked"))

(define (directory-test src)
  (define zd (read-zip-directory src))
  (unless (zip-directory? zd)
    (error "not a zip directory"))
  (define (check-there p)
    (unless (zip-directory-contains? zd p)
      (error 'unzip-test "not there: ~e" p)))
  (check-there "ex1/f1")
  (check-there #"ex1/f1")
  (check-there "ex1/more/f4")
  (check-there (string->path "ex1/more/f4"))
  (unless (zip-directory-includes-directory? zd "ex1/more")
    (error "directory missing"))
  (define (check-not-there p)
    (when (zip-directory-contains? zd p)
      (error "there!"))
    (with-handlers ([exn:fail:unzip:no-such-entry?
                     (lambda (exn)
                       (unless (equal? (exn:fail:unzip:no-such-entry-entry exn)
                                       (if (bytes? p)
                                           p
                                           (path->zip-path p)))
                         (error "bad exn")))])
      (unzip-entry src zd p)))
  (check-not-there #"f1")

  (for ([entry (in-list (zip-directory-entries zd))])
    (parameterize ([current-directory work-dir])
      (unzip-entry src zd entry
                   (make-filesystem-entry-reader #:dest "sub"))))
  (diff ex1-dir (build-path sub-dir "ex1"))
  (delete-directory/files sub-dir))
(directory-test a.zip)
(call-with-input-file a.zip
  directory-test)

(delete-directory/files work-dir)
