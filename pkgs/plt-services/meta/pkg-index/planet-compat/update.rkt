#lang racket/base
(require net/url
         racket/file
         racket/port
         racket/match
         racket/runtime-path
         planet/config
         racket/system
         racket/path
         racket/list
         setup/unpack)

(module+ main
  (require "common.rkt"))

(define (delete-directory/files* p)
  (when (or (file-exists? p) (directory-exists? p))
    (delete-directory/files p)))

(require (prefix-in p:
                    (combine-in
                     planet/private/parsereq
                     planet/private/data)))
(define (substring* s st end)
  (substring s st (+ (string-length s) end)))
(define (remove-suffix s)
  (regexp-replace #rx"\\.([^\\.]*?)$" s ""))
(define (convert-one-planet-req pkgs orig-bs)
  (define orig-bs-i (open-input-bytes orig-bs))
  (define-values (new-byte new-dep)
    (with-handlers ([exn?
                     (λ (x)
                       (eprintf "skipping possible planet dep ~e because of exception ~e\n"
                                orig-bs (exn-message x))
                       (define here (file-position orig-bs-i))
                       (file-position orig-bs-i 0)
                       (values (read-bytes here orig-bs-i)
                               empty))]
                    [list?
                     (λ (x)
                       (apply error x))])
      (define orig-v (read orig-bs-i))
      (define orig-r (p:spec->req orig-v #'error))
      (define spec (p:request-full-pkg-spec orig-r))
      (define user (first (p:pkg-spec-path spec)))
      (define pkg
        (format "~a~a"
                (remove-suffix (p:pkg-spec-name spec))
                (verify-package-exists pkgs spec)))
      (values
       (string->bytes/utf-8
        (format "~a/~a/~a~a"
                user
                pkg
                (if (empty? (p:request-path orig-r))
                  ""
                  (string-append
                   (apply string-append
                          (add-between (p:request-path orig-r) "/"))
                   "/"))
                (remove-suffix (p:request-file orig-r))))
       (list
        (format "planet-~a-~a"
                user pkg)))))
  (define-values (new-bytes new-deps)
    (update-planet-reqs pkgs (port->bytes orig-bs-i)))
  (values (bytes-append
           new-byte new-bytes)
          (append
           new-dep new-deps)))

(define (update-planet-reqs pkgs orig)
  (match (regexp-match-positions #px#"\\(\\s*planet\\s+.*\\s*\\)" orig)
    [#f
     (values orig
             empty)]
    [(cons (cons start end) _)
     (define-values (new-bytes new-deps)
       (convert-one-planet-req pkgs (subbytes orig start)))
     (values (bytes-append (subbytes orig 0 start)
                           new-bytes)
             new-deps)]))

(module+ test
  (require rackunit)
  (define fake-pkgs
    (list (list "mcdonald" "farm.plt" (list 1 0))
          (list "mcdonald" "glue-factory.plt" (list 1 0))))
  (define-syntax-rule (p in exp-bs exp-deps)
    (let ()
      (define-values (act-bs act-deps) (update-planet-reqs fake-pkgs in))
      (check-equal? act-bs exp-bs)
      (check-equal? act-deps exp-deps)))

  (p #"planet mcdonald/farm"
     #"planet mcdonald/farm"
     '())
  (p #"(planet mcdonald/farm)"
     #"mcdonald/farm1/main"
     '("planet-mcdonald-farm1"))
  (p #"ababab (planet mcdonald/farm) ababab"
     #"ababab mcdonald/farm1/main ababab"
     '("planet-mcdonald-farm1"))
  (p #"(planet mcdonald/farm) (planet mcdonald/farm)"
     #"mcdonald/farm1/main mcdonald/farm1/main"
     '("planet-mcdonald-farm1" "planet-mcdonald-farm1"))
  (p #"(planet mcdonald/farm) (planet mcdonald/glue-factory)"
     #"mcdonald/farm1/main mcdonald/glue-factory1/main"
     '("planet-mcdonald-farm1" "planet-mcdonald-glue-factory1"))
  (p #"(planet mcdonald/farm/duck)"
     #"mcdonald/farm1/duck"
     '("planet-mcdonald-farm1"))
  (p #"(planet mcdonald/farm:1)"
     #"mcdonald/farm1/main"
     '("planet-mcdonald-farm1"))
  (p #"(planet mcdonald/farm:1:5)"
     #"mcdonald/farm1/main"
     '("planet-mcdonald-farm1"))
  (p #"(planet mcdonald/farm:1:5/duck)"
     #"mcdonald/farm1/duck"
     '("planet-mcdonald-farm1"))
  (p #"(planet mcdonald/farm/duck/quack)"
     #"mcdonald/farm1/duck/quack"
     '("planet-mcdonald-farm1"))
  (p #"(planet \"mcdonald/farm/duck/quack\")"
     #"mcdonald/farm1/duck/quack"
     '("planet-mcdonald-farm1"))
  (p #"(planet \"quack.rkt\" (\"mcdonald\" \"farm.plt\") \"duck\")"
     #"mcdonald/farm1/duck/quack"
     '("planet-mcdonald-farm1"))
  (p #"(planet \"quack.rkt\" (\"mcdonald\" \"farm.plt\") \"duck\" \"mallard\")"
     #"mcdonald/farm1/duck/mallard/quack"
     '("planet-mcdonald-farm1")))

(module+ main
  (define pkg-info-url
    (string->url "http://planet.racket-lang.org/servlets/pkg-info.ss"))
  (define pkgs
    (call/input-url pkg-info-url get-pure-port (λ (p) (read p) (read p))))
  (define planet-download-url
    (string->url (HTTP-DOWNLOAD-SERVLET-URL))))

(define (verify-package-exists pkgs spec)
  (or
   (for/or ([p (in-list pkgs)])
     (match-define (list user pkg (list max-maj min)) p)
     (and (equal? (p:pkg-spec-name spec) pkg)
          (equal? (p:pkg-spec-path spec) (list user))
          (if (p:pkg-spec-maj spec)
            (and
             ;; This is too restrictive given the number of errors in Planet packages
             (<= (p:pkg-spec-maj spec) max-maj)
             (p:pkg-spec-maj spec))
            max-maj)))
   ;; Hacks
   (let ()
     (define hack-v
       (list (p:pkg-spec-name spec)
             (p:pkg-spec-path spec)
             (p:pkg-spec-maj spec)))
     (cond
       [(member hack-v
                (list
                 ;; These packages straight-up don't exist
                 '("displayz.plt" ("synx") #f) ;; from synx/xml-writer
                 '("galore.plt" ("cce") 1) ;; from soegaard/galore (it's in a comment about avoiding collisions)
                 '("combinators.plt" ("cce") 1) ;; from soegaard/galore (it's in a comment about avoiding collisions)
                 ;; These are errors in a package's documnetation
                 '("aspectscheme.plt" ("dutchyn") 4) ;; maj should be 1
                 '("divascheme.plt" ("dyoo") 1) ;; user should be divascheme
                 '("diff.plt" ("wmfarr") 1) ;; pkg should be deriv
                 '("tetris.plt" ("dvanhorn") 5) ;; maj should be 3
                 '("file-utils.plt" ("erast") #f) ;; pkg should be fileutils
                 ;; These are from packages about planet features
                 '("sqld-psql-ffi.plt" ("planet") 1)
                 '("sqlid.plt" ("planet") 1)
                 '("package-from-local-filesystem.plt" ("fake-author") 1)
                 '("package-from-svn.plt" ("fake-author") 1)
                 '("module.plt" ("package") #f)
                 '("mcfly-scribble.plt" ("~A") #f)
                 '("mcfly-expand.plt" ("~A") #f)
                 '("bar.plt" ("untyped") 1)))
        (error 'verify-package-exists "hack!")]
       [else #f]))
   ;; End Hacks
   (raise (list 'verify-package-exists "Cannot determine newest major for ~e ~e"
                spec
                (list (p:pkg-spec-name spec)
                      (p:pkg-spec-path spec)
                      (p:pkg-spec-maj spec))))))

(module+ main
  (for ([p (in-list pkgs)])
    (match-define (list user pkg (list max-maj min)) p)

    (define last-min-p (build-path mins (format "~a:~a" user pkg)))
    (define last-min
      (if (file-exists? last-min-p)
        (file->value last-min-p)
        -inf.0))
    (define delete?
      (not (= last-min min)))

    (begin0
      (for/list ([maj (in-range 1 (add1 max-maj))])
        (let/ec esc
          (define dl-url
            (struct-copy url planet-download-url
                         [query
                          (let ([get (lambda (access) (format "~s" access))])
                            `((lang   . ,(get (DEFAULT-PACKAGE-LANGUAGE)))
                              (name   . ,(get pkg))
                              (maj    . ,(get maj))
                              (min-lo . "0" #;,(get min))
                              (min-hi . "#f" #;,(get min))
                              (path   . ,(get (list user)))))]))
          (define pkg-short
            (format "~a:~a:~a" user maj pkg))

          (define-syntax-rule (when-delete? e ...)
            (with-handlers ([exn:fail? void])
              (when delete?
                e ...)))

          (define dest
            (build-path orig-pkg pkg-short))
          (when-delete?
           (delete-file dest))
          (unless (file-exists? dest)
            (when #f
              (printf "Downloading ~a\n"
                      pkg-short))
            (define pkg-bs
              (call/input-url dl-url get-impure-port
                              (λ (in)
                                (define hs (purify-port in))
                                (when (string=? "404" (substring hs 9 12))
                                  (esc #f))
                                (port->bytes in))))
            (call-with-output-file dest
              (λ (out) (write-bytes pkg-bs out))))

          (define dest-dir
            (build-path orig pkg-short))
          (when-delete?
           (delete-directory/files dest-dir))
          (unless (directory-exists? dest-dir)
            (printf "Unpacking ~a\n" pkg-short)
            (make-directory dest-dir)
            (unpack dest pkg-dir
                    (lambda (x) (printf "~a\n" x))
                    (lambda () dest-dir)
                    #f
                    (lambda (auto-dir main-dir file) dest-dir)))

          (define pkg/no-plt
            (format "~a~a"
                    (regexp-replace* #rx"\\.plt$" pkg "")
                    maj))
          (define pkg-name
            (format "planet-~a-~a" user pkg/no-plt))
          (define pkg-name.plt
            (format "~a.plt" pkg-name))
          (define pkg-dir
            (build-path work pkg-name))

          (when-delete?
           (delete-directory/files pkg-dir))

          (with-handlers
              ([exn? (λ (x)
                       (delete-directory/files pkg-dir)
                       (raise x))])
            (unless (directory-exists? pkg-dir)
              (printf "Translating ~a\n" pkg-short)
              (make-directory* pkg-dir)
              (define files-dir
                (build-path pkg-dir user pkg/no-plt))
              (make-directory* files-dir)

              (define all-deps
                (fold-files
                 (λ (p ty deps)
                   (define rp
                     (find-relative-path dest-dir p))
                   (define fp
                     (if (equal? p rp)
                       files-dir
                       (build-path files-dir rp)))
                   (match ty
                     ['dir
                      (make-directory* fp)
                      deps]
                     ['file
                      (match (filename-extension rp)
                        [(or #"ss" #"scrbl" #"rkt" #"scs" #"scm" #"scribl")
                         (define orig (file->bytes p))
                         (define-values (changed new-deps)
                           (update-planet-reqs pkgs orig))
                         (display-to-file changed fp)
                         (append new-deps deps)]
                        [_
                         (copy-file p fp)
                         deps])]))
                 empty
                 dest-dir
                 #f))
              (define deps
                (remove pkg-name
                        (remove-duplicates
                         all-deps)))

              (printf "\tdeps ~a\n" deps)
              (call-with-output-file*
                  (build-path pkg-dir "info.rkt")
                (lambda (o)
                  (fprintf o "#lang info\n")
                  (write `(define collection 'multi) o)
                  (write `(define deps ',deps) o)))))


          (define pkg-pth (build-path pkg-depo pkg-depo-dir pkg-name.plt))
          (when-delete?
           (delete-file pkg-pth)
           (delete-file (string-append (path->string pkg-pth) ".CHECKSUM")))
          (unless (file-exists? pkg-pth)
            (printf "Packaging ~a\n" pkg-short)
            (parameterize ([current-directory work])
              (system (format "raco pkg create --format plt \"~a\"" pkg-name))
              (rename-file-or-directory
               (build-path work pkg-name.plt)
               pkg-pth)
              (rename-file-or-directory
               (string-append (path->string (build-path work pkg-name.plt)) ".CHECKSUM")
               (string-append (path->string pkg-pth) ".CHECKSUM"))))

          pkg-name))

      (write-to-file min last-min-p
                     #:exists 'replace))))
