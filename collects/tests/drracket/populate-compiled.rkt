#lang racket/base
(require racket/file
         racket/system
         compiler/find-exe
         pkg/lib)

(unless (eq? 'user (default-pkg-scope))
  (error "Run this test with `user' default package scope"))

(define dir (make-temporary-file "~a" 'directory))
(define pkg-dir (build-path dir "popcomp-pkg"))
(define coll-dir (build-path pkg-dir "popcomp"))
(define pkg2-dir (build-path dir "popcomp2-pkg"))
(define coll2-dir (build-path pkg2-dir "popcomp2"))

(make-directory* coll-dir)
(make-directory* coll2-dir)

(call-with-output-file*
 (build-path coll-dir "main.rkt")
 (lambda (o)
   (fprintf o "#lang racket/base\n")
   (write '(provide popcomp) o)
   (write '(define popcomp (gensym 'popcomp)) o)))

(call-with-output-file*
 (build-path coll2-dir "main.rkt")
 (lambda (o)
   (fprintf o "#lang racket/base\n")
   (write '(provide popcomp2) o)
   (write '(define popcomp2 (gensym 'popcomp2)) o)))

(call-with-output-file*
 (build-path dir "y.rkt")
 (lambda (o)
   (fprintf o "#lang racket/base\n")
   (write '(provide y) o)
   (write '(define y (gensym 'y)) o)))

(call-with-output-file*
 (build-path dir "x.rkt")
 (lambda (o)
   (fprintf o "#lang racket/base\n")
   (write '(require "y.rkt" popcomp popcomp2) o)))

(define (system*/error . args)
  (unless (apply system* args)
    (error "failed")))

(define addon-dir (build-path dir "addon"))
(make-directory* addon-dir)
(void (putenv "PLTADDONDIR" (path->string addon-dir)))

(system*/error (find-exe) 
               "-l" "raco" "pkg" "install" "--no-setup" "--link"
               pkg-dir
               pkg2-dir)
(system*/error (find-exe) 
               "-e" 
               "(require (submod tests/drracket/populate-compiled go))"
               (build-path dir "x.rkt"))

(delete-directory/files dir)

;; ----------------------------------------

(module go racket/base
  (require "private/drracket-test-util.rkt"
           racket/gui/base
           racket/class
           racket/path
           racket/file
           framework/test)

  (define (check-compiled compiled? path)
    (unless (equal? compiled? (file-exists? path))
      (error 'check-compiled
             "expected ~acompiled: ~a"
             (if compiled? "" "not ")
             path)))
  
  (fire-up-drracket-and-run-tests 
   (λ ()
      (let ([drs (wait-for-drracket-frame)])
        (define x (vector-ref (current-command-line-arguments) 0))
        (define dir (path-only x))
        
        (do-execute drs)

        (define popcomp-main-zo
          (build-path dir "popcomp-pkg" "popcomp" "compiled" "drracket" "errortrace" "main_rkt.zo"))
        (define popcomp2-main-zo
          (build-path dir "popcomp2-pkg" "popcomp2" "compiled" "drracket" "errortrace" "main_rkt.zo"))

        (check-compiled #t (build-path dir "compiled" "drracket" "errortrace" "y_rkt.zo"))
        (check-compiled #f popcomp-main-zo)
        (check-compiled #f popcomp2-main-zo)

        ;; Create a broken ".zo" file where it should not be used:
        (make-directory* (path-only popcomp-main-zo))
        (call-with-output-file* 
         popcomp-main-zo
         (lambda (o)
           (fprintf o "broken\n")))

        (do-execute drs)
        (let* ([got (fetch-output drs)])
          (unless (string=? "" got)
            (error 'check-output "wrong output: ~s" got)))

        (delete-file popcomp-main-zo)

        ;; Open "main.rkt" in "popcomp-pkg", so now it should be compiled
        ;; when we run "x.rkt":

        (test:menu-select "File" "New Tab")
        (use-get/put-dialog (λ () 
                               (test:menu-select "File" "Open..."))
                            (build-path dir "popcomp-pkg" "popcomp" "main.rkt"))

        (queue-callback/res (λ () (send drs change-to-tab (car (send drs get-tabs)))))

        (do-execute drs)

        (check-compiled #t popcomp-main-zo)
        (check-compiled #f popcomp2-main-zo)

        ;; But if the "popcomp-pkg" directory is not writable, then
        ;; don't compile after all:

        (delete-file popcomp-main-zo)
        (file-or-directory-permissions (build-path dir "popcomp-pkg") #o555)
        
        (do-execute drs)

        (check-compiled #f popcomp-main-zo)
        (check-compiled #f popcomp2-main-zo)

        (file-or-directory-permissions (build-path dir "popcomp-pkg") #o777)))))
