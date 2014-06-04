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
        (and (= (file-or-directory-modify-seconds p1)
                (file-or-directory-modify-seconds p2))
             (equal? (file-or-directory-permissions p1)
                     (file-or-directory-permissions p2)))))
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

(define (zip-tests zip unzip)
  (make-directory* "ex1")
  (make-file (build-path "ex1" "f1"))
  (make-file (build-path "ex1" "f2"))
  (make-file (build-path "ex1" "f3"))
  (define more-dir (build-path "ex1" "more"))
  (make-directory* more-dir)
  (make-file (build-path more-dir "f4"))

  (zip "a.zip" "ex1")

  (make-directory* "sub")
  (parameterize ([current-directory "sub"])
    (unzip "../a.zip"))
  
  (unless (diff "ex1" (build-path "sub" "ex1") #t)
    (eprintf "changed! ~s" zip))

  (delete-directory/files "sub")
  (delete-file "a.zip")

  (zip "a.zip" #:path-prefix "inside" "ex1")
  (make-directory* "sub")
  (parameterize ([current-directory "sub"])
    (unzip "../a.zip"))

  (unless (diff "ex1" (build-path "sub" "inside" "ex1") #t)
    (eprintf "changed! ~s" zip))
  
  (delete-file "a.zip")
  (delete-directory/files "sub")
  (delete-directory/files "ex1"))

(define work-dir (make-temporary-file "packer~a" 'directory))

(parameterize ([current-directory work-dir])
  (zip-tests zip unzip)
  (zip-tests tar untar))

(delete-directory/files work-dir)

'ok
