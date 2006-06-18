;; This file is used to copy the PLT tree as part of `make install'.  There is
;; no good cmdline interface, since it is internal, and should be as
;; independent as possible.  Expects these arguments:
;; * The source plt directory
;; * Path names that should be copied (bin, collects, doc, lib, ...)
;; * A boolean "yes"/"no" flag indicating if an original tree structure is used
;; >>> Should be merged into plt/collects/setup/unixstyle-install.ss
(module copytree mzscheme

  (define args (vector->list (current-command-line-arguments)))

  (define (get-arg)
    (when (null? args) (error "insufficient arguments"))
    (begin0 (car args) (set! args (cdr args))))

  (define pltdir        (get-arg))
  (define bindir        (get-arg))
  (define collectsdir   (get-arg))
  (define docdir        (get-arg))
  (define libdir        (get-arg))
  (define includepltdir (get-arg))
  (define libpltdir     (get-arg))
  (define mandir        (get-arg))
  (define origtree      (get-arg))

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
