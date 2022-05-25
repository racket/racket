#lang racket/base
(require (for-syntax racket/base)
         racket/file
         racket/rerequire
         compiler/cm
         compiler/compilation-path
         rackunit)

(use-compiled-file-check 'modify-seconds) ; in case it isn't


(define (member/compiled path changed-paths)
  (member (get-compilation-bytecode-file path) changed-paths))


(define (compile-file path)
  (parameterize ([current-namespace (make-base-empty-namespace)] ;; omits deps otherwise???
                 [compile-enforce-module-constants #f])          ;; no reloading allowed otherwise
    (managed-compile-zo path)))

(define-syntax (do-test stx)
  (syntax-case stx ()
    [(_ name zero-src? one-src? two-src/1? two-src/2? initial-value updated-value)
     (syntax/loc stx
       (test-case
        name
        (define dir0 (make-temporary-file "rereq-~a" 'directory))
        (define dir1 (make-temporary-file "rereq-~a" 'directory))
        (define dir2 (make-temporary-file "rereq-~a" 'directory))
        (define zero-path (build-path dir0 "zero.rkt"))
        (define one-path (build-path dir1 "one.rkt"))
        (define two-path (build-path dir2 "two.rkt"))
   
        (define (create-zero)
          (display-to-file
           (string-append
            "#lang racket/base\n"
            (format "(require (file ~v))"
                    (path->string (build-path dir1 "one.rkt")))
            "(provide proc)")
           zero-path
           #:exists 'replace))


        (define (create-one)
          (display-to-file
           (string-append
            "#lang racket/base\n"
            "(module inner racket/base\n"
            (format "  (require (file ~v))\n"
                    (path->string two-path))
            "  (provide proc))\n"
            "(module more-inner racket/base\n"
            "  (require (submod \"..\" inner))\n"
            "  (provide proc))\n"
            "(require 'more-inner)\n"
            "(provide proc)")
           one-path
           #:exists 'replace))


        (define (delete-extra-files dirpath filepath)
          (for ([file (in-list
                       (find-files (lambda (fpath)
                                     (and (not (equal? fpath filepath))
                                          (eq? 'file (file-or-directory-type fpath))))
                                   dirpath))])
            (delete-file file)))

        (define (create-or-change-required-file proc-value recreate?)
          (display-to-file
           (string-append
            "#lang racket/base\n"
            "(module other racket/base)\n"
            "(provide proc)\n"
            (format "(define (proc) ~v)" proc-value))
           two-path
           #:exists 'replace)
          (when recreate? (sleep 1)) ; to make sure mod time changes so rerequire notices it
          (file-or-directory-modify-seconds
           two-path
           (current-seconds)))

        

        (define (make-check-loaded-files two?)
          (lambda (list-of-paths)
            (and
             (if zero-src?
                 (member zero-path list-of-paths)
                 (member/compiled zero-path list-of-paths))
             (if one-src?
                 (member one-path list-of-paths)
                 (member/compiled one-path list-of-paths))
             (if two?
                 (member two-path list-of-paths)
                 (member/compiled two-path list-of-paths)))))


        (define check-loaded-files/initial (make-check-loaded-files two-src/1?))
        (define check-loaded-files/updated (make-check-loaded-files two-src/2?))
        ;; create files, compile them as need be
        (create-zero)
        (create-one)
        (create-or-change-required-file initial-value #f)
        (if zero-src?
            (if one-src?
                (if two-src/1? (void) (compile-file two-path))
                (compile-file one-path))
            (compile-file zero-path))
       (when (and one-src? (not zero-src?)) ; compiling zero made compiled files, delete them
          (delete-extra-files dir1 one-path))
        (when (and two-src/1? (or (not zero-src?)
                                  (not one-src?)))
          (delete-extra-files dir2 two-path))
       
        (define loaded-files (dynamic-rerequire zero-path #:verbosity 'none))
        (define initial-value/actual ((dynamic-require zero-path 'proc)))

        (check-pred check-loaded-files/initial
                    loaded-files
                    "first rerequire loaded wrong modules")
        (check-equal? initial-value/actual
                      initial-value
                      "first rerequire loaded incorrect proc")
        
        (create-or-change-required-file updated-value #t)
        (unless two-src/2?
          (compile-file two-path))
        (define reloaded-files (dynamic-rerequire zero-path #:verbosity 'none))
        (define updated-value/actual ((dynamic-require zero-path 'proc)))
        (check-pred check-loaded-files/updated
                    reloaded-files
                    "second rerequire reloaded wrong modules")
        (check-equal? updated-value/actual
                      updated-value
                      "second rerequire loaded incorrect proc")))]))
        

(do-test "all source" #t #t #t #t "foo" "zam")
(do-test "two compiled for reload" #t #t #t #f "foo" "zam")
(do-test "two compiled for initial" #t #t #f #t "foo" "zam")
(do-test "two compiled" #t #t #f #f "foo" "zam")

(do-test "one compiled" #t #f #t #t "foo" "zam")
(do-test "one compiled, two compiled for reload" #t #f #t #f "foo" "zam")
(do-test "one compiled, two compiled for initial" #t #f #f #t "foo" "zam")
(do-test "one compiled, two compiled" #t #f #f #f "foo" "zam")

(do-test "zero compiled" #f #t #t #t "foo" "zam")
(do-test "zero compiled, two compiled for reload" #f #t #t #f "foo" "zam")
(do-test "zero compiled, two compiled for initial" #f #t #f #t "foo" "zam")
(do-test "zero compiled, two compiled" #f #t #f #f "foo" "zam")

(do-test "zero compiled, one compiled" #f #f #t #t "foo" "zam")
(do-test "zero compiled, one compiled, two compiled for reload" #f #f #t #f "foo" "zam")
(do-test "zero compiled, one compiled, two compiled for initial" #f #f #f #t "foo" "zam")
(do-test "all compiled" #f #f #f #f "foo" "zam")


(test-case "up-paths"
  (define dir (make-temporary-file "rereq-~a" 'directory))
  (define subdir1 (build-path dir "a"))
  (define subdir2 (build-path dir "b"))
  (make-directory subdir1)
  (make-directory subdir2)
  (define path1 (build-path subdir1 "a.rkt"))
  (define path2 (build-path subdir2 "b.rkt"))

  (display-to-file
   #:exists 'replace
   #<<CODE
#lang racket/base
(provide proc)
(define (proc) 1)
CODE
   path1)

  (display-to-file
   #:exists 'replace
   #<<CODE
#lang racket/base
(require "../a/a.rkt")
(provide proc)
CODE
   path2)

  (define (reload)
    (dynamic-rerequire path2 #:verbosity 'none)
    (dynamic-require path2 'proc))

  (check-equal? ((reload)) 1)

  (sleep 1)
  (display-to-file
   #:exists 'replace
   #<<CODE
#lang racket/base
(provide proc)
(define (proc) 2)
CODE
   path1)

  (check-equal? ((reload)) 2))


(test-case "ordering"
  (define dir (make-temporary-file "rereq-~a" 'directory))
  (define path1 (build-path dir "a.rkt"))
  (define path2 (build-path dir "b.rkt"))
  (define path3 (build-path dir "c.rkt"))
  (define path4 (build-path dir "d.rkt"))

  (display-to-file
   #:exists 'replace
   #<<CODE
#lang racket/base
(provide proc)
(define (proc) 1)
CODE
   path1)

  (display-to-file
   #:exists 'replace
   #<<CODE
#lang racket/base
(require "a.rkt")
(provide proc-b)
(define proc-b proc)
CODE
   path2)

  (display-to-file
   #:exists 'replace
   #<<CODE
#lang racket/base
(require "a.rkt")
(provide proc-c)
(define proc-c proc)
CODE
   path3)

  (display-to-file
   #:exists 'replace
   #<<CODE
#lang racket/base
(require "b.rkt"
         "c.rkt")
(provide proc-b
         proc-c)
CODE
   path4)

  (define (reload)
    (dynamic-rerequire path4 #:verbosity 'none)
    (values
     (dynamic-require path4 'proc-b)
     (dynamic-require path4 'proc-c)))

  (let-values ([(proc-b proc-c) (reload)])
   (check-equal? (proc-b) 1)
   (check-equal? (proc-c) 1))

  (sleep 1)
  (display-to-file
   #:exists 'replace
   #<<CODE
#lang racket/base
(provide proc)
(define (proc) 2)
CODE
   path1)

  (let-values ([(proc-b proc-c) (reload)])
   (check-equal? (proc-b) 2)
   (check-equal? (proc-c) 2)))
