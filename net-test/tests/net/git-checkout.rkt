#lang racket/base
(require net/git-checkout
         racket/file
         racket/system
         web-server/servlet-env)

(define git-exe
  (find-executable-path (if (eq? 'windows (system-type))
                            "git.exe"
                            "git")))

(define (git . args)
  (unless (apply system* git-exe args)
    (error "failed")))

(define (make-file p content)
  (call-with-output-file
   p
   #:exists 'truncate
   (lambda (o) (write-bytes content o))))

(define (filter-source l)
  (filter (lambda (p) (not (equal? p (string->path ".git")))) l))

(define (compare a b)
  (cond
   [(link-exists? a)
    (unless (link-exists? b)
      (error 'compare "not both links: ~s and ~s" a b))
    (define ra (resolve-path a))
    (define rb (resolve-path b))
    (unless (equal? ra rb)
      (error 'compare "different link targets: ~s to ~s vs. ~s to ~s"
             a ra b rb))]
   [(directory-exists? a)
    (unless (directory-exists? b)
      (error 'compare "not both dirs: ~s and ~s" a b))
    (define al (filter-source (directory-list a)))
    (define bl (directory-list b))
    (unless (equal? al bl)
      (error 'compare "different content: ~s with ~s versus ~s with ~s"
             a al b bl))
    (for ([ap (in-list al)]
          [bp (in-list bl)])
      (compare (build-path a ap) (build-path b bp)))]
   [(file-exists? a)
    (unless (file-exists? b)
      (error 'compare "not both files: ~s and ~s" a b))
    (define ba (file->bytes a))
    (define bb (file->bytes b))
    (unless (equal? ba bb)
      (error 'compare "different file content: ~s to ~s vs. ~s to ~s"
             a ba b bb))
    (define pa (file-or-directory-permissions a))
    (define pb (file-or-directory-permissions b))
    (unless (equal? pa pb)
      (error 'compare "different file permissions: ~s to ~s vs. ~s to ~s"
             a pa b pb))]
   [else
    (error 'compare "no such file: ~s" a)]))

(when git
  (define dir (make-temporary-file "~a-git-test" 'directory))
  (define http-custodian (make-custodian))
  (dynamic-wind
   void
   (lambda () 
     (parameterize ([current-custodian http-custodian])
     (thread
      (lambda ()
        (serve/servlet
         void
         #:command-line? #t
         #:extra-files-paths (list dir)
         #:servlet-regexp #rx"$." ; no servlets
         #:port 8950))))
     
     (parameterize ([current-directory dir])
       (make-directory "repo")
       (parameterize ([current-directory "repo"])
         (make-file "x" #"hello")
         (make-file "y" #"goodbye")
         (unless (eq? (system-type) 'windows)
           (file-or-directory-permissions "y" #o755))
         (make-file "z" #"whatever")
         (unless (eq? (system-type) 'windows)
           (file-or-directory-permissions "z" #o644))
         (unless (eq? (system-type) 'windows)
           (make-file-or-directory-link "x" "also-x"))
         (git "init")
         (git "add" ".")
         (git "commit" "-m" "initial commit")
         (git "update-server-info"))
       
       (git-checkout "localhost" #:port 8950 #:transport 'http
                     "repo/.git"
                     #:dest-dir "also-repo")
       
       (compare "repo" "also-repo")
       
       (void)))
   (lambda ()
     (custodian-shutdown-all http-custodian)
     (delete-directory/files dir))))

  
