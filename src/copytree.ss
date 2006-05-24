;; This file is used to copy the PLT tree as part of `make install', and as
;; part of Unix installers.  It should be invoked with the source plt directory
;; (holding a usual plt tree), and a list of path names that should be copied.
;; Not providing a good cmdline interface, since it is should be as independent
;; as possible.
(module copytree mzscheme

  (define args (vector->list (current-command-line-arguments)))

  (define (path-arg)
    (when (null? args) (error "insufficient arguments"))
    (begin0 (car args) (set! args (cdr args))))

  (define pltdir        (path-arg))
  (define bindir        (path-arg))
  (define collectsdir   (path-arg))
  (define docdir        (path-arg))
  (define libdir        (path-arg))
  (define includepltdir (path-arg))
  (define libpltdir     (path-arg))
  (define mandir        (path-arg))
  (define origtree      (path-arg))

  (define (skip-name? n)
    (regexp-match #rx#"^(?:[.]svn|CVS|compiled)$" (path->bytes n)))

  (define (copytree src dest)
    (let ([src (simplify-path src #f)])
      (printf "Copying ~a -> ~a\n" src dest)
      (let loop ([src src] [dest dest])
        (for-each (lambda (n)
                    (unless (skip-name? n)
                      (let ([from (build-path src  n)]
                            [to   (build-path dest n)])
                        (cond
                         [(file-exists? from)
                          (when (file-exists? to) (delete-file to))
                          (copy-file from to)]
                         [(directory-exists? from)
                          (unless (directory-exists? to) (make-directory to))
                          (copytree from to)])
                        (let ([t (file-or-directory-modify-seconds from)])
                          (file-or-directory-modify-seconds to t)))))
                  (directory-list src)))))

  (copytree (build-path pltdir "collects") collectsdir)
  (copytree (build-path pltdir "doc") docdir)
  (copytree (build-path pltdir "man") mandir)

  (unless (equal? origtree "yes")
    ;; Replace "config.ss"
    (with-output-to-file (build-path collectsdir "config" "config.ss")
      (lambda ()
	(printf "(module config (lib \"configtab.ss\" \"setup\")\n")
	(printf "  (define doc-dir ~s)\n" docdir)
	(when (eq? 'shared (system-type 'link))
	  (printf "  (define dll-dir ~s)\n" libdir))
	(printf "  (define lib-dir ~s)\n" libpltdir)
	(printf "  (define include-dir ~s)\n" includepltdir)
	(printf "  (define bin-dir ~s)\n" bindir)
        (printf "  (define absolute-installation? #t))\n"))
      'truncate/replace))

  )
