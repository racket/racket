#lang racket/base
(require file/untar
         racket/file
         racket/system)

(define tmp (find-system-path 'temp-dir))
(define tar-exe (find-executable-path "tar"))

(define work-dir (build-path tmp (format "untar-testing~a" (random 1000))))
(printf "Working in ~a\n" work-dir)
(when (directory-exists? work-dir)
  (delete-directory/files work-dir))
(define a.tar (build-path work-dir "a.tar"))

(define sub-dir (build-path work-dir "sub"))

(define (make-file path mod-time [permissions '(read write)])
  (with-output-to-file path
    (lambda ()
      (write-bytes (make-bytes (random 100000)))))
  (file-or-directory-modify-seconds path mod-time)
  (file-or-directory-permissions* path permissions))

(define (file-or-directory-permissions* path permissions)
  (file-or-directory-permissions path 
                                 (bitwise-ior
                                  (if (memq 'read permissions)
                                      #o444
                                      0)
                                  (if (memq 'write permissions)
                                      #o222
                                      0)
                                  (if (memq 'execute permissions)
                                      #o111
                                      0))))

(define ex1-dir (build-path work-dir "ex1"))

(make-directory* ex1-dir)
(make-file (build-path ex1-dir "f1") (- (current-seconds) 12))
(make-file (build-path ex1-dir "f2") (+ (current-seconds) 12) '(read write execute))
(make-file (build-path ex1-dir "f3") (- (current-seconds) 7) '(read))
(make-file-or-directory-link "fnone" (build-path ex1-dir "f4"))
(define more-dir (build-path ex1-dir "more"))
(make-directory* more-dir)
(make-file (build-path more-dir "f4") (current-seconds))
(file-or-directory-permissions* more-dir '(read execute)) ; not 'write

(define (tar dir)
  (define-values (base name dir?) (split-path dir))
  (parameterize ([current-directory base])
    (void (system* tar-exe "-c" "-f" a.tar name))))

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
                 (= (file-or-directory-modify-seconds src)
                    (file-or-directory-modify-seconds dest))
                 (equal? (file-or-directory-permissions src)
                         (file-or-directory-permissions dest))
                 (equal? (file->bytes src) (file->bytes dest)))
      (diff-error src dest))]
   [(directory-exists? src)
    (unless (and (directory-exists? dest)
                 (= (file-or-directory-modify-seconds src)
                    (file-or-directory-modify-seconds dest))
                 (equal? (file-or-directory-permissions src)
                         (file-or-directory-permissions dest)))
      (diff-error src dest))
    (define (sort-paths l)
      (sort l bytes<? #:key path->bytes))
    (define srcs (sort-paths (directory-list src)))
    (define dests (sort-paths (directory-list dest)))
    (unless (equal? srcs dests) (diff-error src dest))
    (for ([src-item (in-list srcs)]
          [dest-item (in-list dests)])
      (diff (build-path src src-item) (build-path dest dest-item)))
    ;; make dest writable to simplify clean-up:
    (file-or-directory-permissions* dest '(read execute write))]
   [else (void)]))

(tar ex1-dir)

(make-directory* sub-dir)
(parameterize ([current-directory sub-dir])
  (untar a.tar))
(diff ex1-dir (build-path sub-dir "ex1"))
(delete-directory/files sub-dir)

(parameterize ([current-directory work-dir])
  (untar a.tar #:dest "sub"))
(diff ex1-dir (build-path sub-dir "ex1"))
(delete-directory/files sub-dir)

(parameterize ([current-directory work-dir])
  (untar a.tar #:dest "sub" #:filter (lambda args #f)))
(when (directory-exists? sub-dir)
  (error "should not have been unpacked"))

(file-or-directory-permissions* more-dir '(read execute write))
(delete-directory/files work-dir)
