;; This file is used to move the Racket tree as part of a Unix sh-installer
;; (when it works in unix-style mode) and similar situations.  When possible
;; (`move' mode), this is done carefully (undoing changes if there is an
;; error), and a racket-uninstall script is generated.  It is also used to
;; change an already existing tree (eg, when DESTDIR is used) and to copy a
;; tree (possibly part of `make install').  There is no good cmdline interface,
;; since it is internal, and should be as independent as possible (it moves the
;; collection tree).  Expects these arguments:
;; * An operation name:
;;   - `move': move a relative installation from `rktdir' to an absolute
;;     installation in the given paths (used by the shell installers)
;;     (interactive, undo-on-error, create-uninstaller)
;;   - `copy': similar to `move', but copies instead of moving
;;   - `make-install-copytree': copies some toplevel directories, skips ".*"
;;     and "compiled" subdirs, and rewrites "config.rkt", but no uninstaller
;;     (used by `make install') (requires an additional `origtree' argument)
;;   - `make-install-destdir-fix': fixes paths in binaries, laucnhers, and
;;     config.rkt (used by `make install' to fix a DESTDIR) (requires exactly
;;     the same args as `make-install-copytree' (prefixed) and requires a
;;     DESTDIR setting)
;; * rktdir: The source racket directory
;; * Path names that should be moved/copied (bin, collects, doc, lib, ...)

#lang racket/base

(define args (vector->list (current-command-line-arguments)))

(define (get-arg)
  (when (null? args) (error "insufficient arguments"))
  (begin0 (car args) (set! args (cdr args))))

(define op (string->symbol (get-arg)))
(define rktdir (get-arg))
(define dirs (map (lambda (name) (list name (get-arg)))
                  '(bin collects doc lib includerkt librkt man #|src|#)))

(define (dir: name)
  (cadr (or (assq name dirs) (error 'getdir "unknown dir name: ~e" name))))

;; Configures level where we start owning stuff (in the sense that the
;; generated uninstaller will remove it, and the installation will remove dirs
;; instead of moving into them).  For example, a 1-level for collectsdir means
;; that we will overwrite collects/foo instead of adding files in it.  A level
;; of 999 means that we always add stuff if the directory exists; a level of 0
;; means that we always own the directory (should never be used).  In any case,
;; we start own stuff (= add it to the uninstaller) once we encounter a
;; directory that does not already exist.  #f means that we never own
;; directories, only files.
(define (level-of dir)
  (let ([dir (string->symbol (basename dir))])
    (case dir
      [(bin)      #f]
      [(collects) 1]
      [(doc)      1]
      [(include)  1]
      ;; if shared libraries are used, then these files should be moved
      ;; independently, as if they had a level of #f
      [(lib)      1]
      [(man)      #f]
      [(src)      1]
      [(README)   #f] ; moved last
      [else (error 'level-of "internal-error -- unknown dir: ~e" dir)])))

(define (make-path . args) ; like build-path but returns a string
  (path->string (apply build-path args)))

(define (basename path) ; returns a string
  (let-values ([(dir name dir?) (split-path path)]) (path->string name)))

(define (dirname path) ; returns a string
  (let-values ([(dir name dir?) (split-path path)]) dir))

;; like directory-list, but returns a sorted list of strings (this is a lot of
;; code just to get the sorting, but it's better if an installer operates in a
;; deterministic way)
(define (ls . dir)
  (sort (map path->string (apply directory-list dir)) string<?))

;; convenient wrapper for a simple subprocess
(define (run cmd . args)
  (let-values
      ([(p _1 _2 _3)
        (apply subprocess
               (current-output-port) (current-input-port) (current-error-port)
               (find-executable-path cmd) args)])
    (subprocess-wait p)
    (unless (zero? (subprocess-status p))
      (error (format "~a: returned an error exit code"
                     (let ([s (format "~a" (cons cmd args))])
                       (substring s 1 (sub1 (string-length s)))))))))

;; removes a file or a directory (recursively)
(define (rm path)
  (cond [(or (file-exists? path) (link-exists? path)) (delete-file path)]
        [(directory-exists? path)
         (parameterize ([current-directory path]) (for-each rm (ls)))
         (delete-directory path)]
        [else #t])) ; shouldn't happen

;; used for filtering files when copying (and moving toplevels)
(define skip-filter (lambda (p) #f))

;; copy a file or a directory (recursively), preserving time stamps
;; (racket's copy-file preservs permission bits)
(define (cp src dst)
  (let loop ([src src] [dst dst])
    (let ([time! (lambda ()
                   (file-or-directory-modify-seconds
                    dst (file-or-directory-modify-seconds src)))])
      (cond [(skip-filter src) 'skip]
            [(link-exists? src)
             (make-file-or-directory-link (resolve-path src) dst)]
            [(directory-exists? src)
             (make-directory dst) (time!)
             (parameterize ([current-directory src])
               (for-each (lambda (p) (loop p (make-path dst p))) (ls)))]
            [(file-exists? src) (copy-file src dst) (time!)]
            [else (error 'cp "internal error: ~e" src)]))))

;; try to rename and if it fails (due to different fs) copy and remove
(define (mv src dst)
  (unless (with-handlers ([(lambda (e)
                             (and (exn:fail:filesystem? e)
                                  (not (exn:fail:filesystem:exists? e))))
                           (lambda (e) #f)])
            (rename-file-or-directory src dst) #t)
    ;; move failed: copy & remove
    (with-handlers ([exn? (lambda (e)
                            ;; error => remove new copy (if can) and re-raise
                            (with-handlers ([exn? (lambda (e) #f)])
                              (rm dst)
                              (raise e)))])
      ;; (cp src dst) (rm src)
      ;; can't do all in Scheme, run mv instead
      (run "mv" "--" src dst))))

;; list of changes, so we can undo them in case of an error and so we can
;; create an uninstaller
(define path-changes '())
(define (register-change! op . args)
  (set! path-changes (cons (cons op args) path-changes)))

;; like `mv', but also record moves
(define (mv* src dst)
  (mv src dst)
  (register-change! 'mv src dst))

;; like `cp', but also record copies
(define (cp* src dst)
  (cp src dst)
  (register-change! 'cp src dst))

(define (fix-executable file)
  (define (fix-binary file)
    (let-values ([(i o) (open-input-output-file file #:exists 'update)])
      (let ([m (regexp-match-positions #rx#"coLLECTs dIRECTORy:" i)])
        (unless m
          (error
           (format "could not find collection-path label in executable: ~a"
                   file)))
        (file-position o (cdar m))
        (display (dir: 'collects) o)
        (write-byte 0 o)
        (write-byte 0 o)
        (close-input-port i)
        (close-output-port o))))
  (define (fix-script file)
    (let* ([size (file-size file)]
           [buf (with-input-from-file file (lambda () (read-bytes size)))]
           [m (or (regexp-match-positions
                   #rx#"\n# {{{ bindir\n(.*?\n)# }}} bindir\n" buf)
                  (error (format "could not find binpath block in script: ~a"
                                 file)))])
      ;; 'truncate file to keep it executable
      (with-output-to-file file #:exists 'truncate
        (lambda ()
          (write-bytes buf (current-output-port) 0 (caadr m))
          (printf "bindir=\"~a\"\n"
                  (regexp-replace* #rx"[\"`'$\\]" (dir: 'bin) "\\\\&"))
          (write-bytes buf (current-output-port) (cdadr m))))))
  (let ([magic (with-input-from-file file (lambda () (read-bytes 10)))])
    (cond [(or (regexp-match #rx#"^\177ELF" magic)
               (regexp-match #rx#"^\316\372\355\376" magic)
               (regexp-match #rx#"^\317\372\355\376" magic))
           (let ([temp (format "~a-temp-for-install"
                               (regexp-replace* #rx"/" file "_"))])
             (with-handlers ([exn? (lambda (e) (delete-file temp) (raise e))])
               ;; always copy so we never change the running executable
               (rm temp)
               (copy-file file temp)
               (fix-binary temp)
               (delete-file file)
               (mv temp file)))]
          [(regexp-match #rx#"^#!/bin/sh" magic)
           (fix-script file)]
          [else (error (format "unknown executable: ~a" file))])))

(define (fix-executables bindir librktdir [binfiles #f])
  (parameterize ([current-directory bindir])
    (for ([f (in-list (or binfiles (ls)))] #:when (file-exists? f))
      (fix-executable f)))
  ;; fix the starter executable too
  (parameterize ([current-directory librktdir])
    (when (file-exists? "starter") (fix-executable "starter"))))

;; remove and record all empty dirs
(define (remove-empty-dirs dir)
  (let loop ([dir dir] [recurse? #t])
    (when (and (directory-exists? dir) (not (link-exists? dir)))
      (let ([ps (ls dir)])
        (cond [(null? ps)
               (delete-directory dir)
               (register-change! 'rd dir)]
              [recurse?
               (for-each (lambda (p) (loop (make-path dir p) #t)) ps)
               (loop dir #f)] ; try again
              ;; get here only on the 2nd round, so we cannot remove it
              )))))

;; called from an error handler, so avoid raising more errors
(define (undo-changes)
  (printf "*** Error: undoing changes...\n")
  (for-each
   (lambda (p)
     (apply (case (car p)
              [(cp) (lambda (src dst) (rm dst))]
              [(mv) (lambda (src dst)
                      (with-handlers ([exn?
                                       (lambda (e)
                                         (eprintf "  ** error during undo: ~a\n"
                                                  (exn-message e))
                                         #f)])
                        (mv dst src)))]
              [(rd) make-directory]
              [(md) delete-directory]
              [(file) delete-file]
              [else (error 'undo-changes "internal-error: ~e" p)])
            (cdr p)))
   path-changes))

(define (write-uninstaller)
  (define uninstaller (make-path (dir: 'bin) "racket-uninstall"))
  (printf "Writing uninstaller at: ~a...\n" uninstaller)
  (register-change! 'file uninstaller)
  (with-output-to-file uninstaller #:exists 'replace
    (lambda ()
      (printf "#!/bin/sh\n")
      (printf "\n# Remove files and dirs that we own\n")
      (printf "while true; do read R || break; rm -rf -- \"$R\"; done \\\n")
      (printf "<<::://E//O//F////O//N//E//:::\n")
      ;; only moved/copied stuff are part of the distribution
      (for ([p path-changes] #:when (memq (car p) '(mv cp)))
        (printf "~a\n" (caddr p)))
      (printf "::://E//O//F////O//N//E//:::\n")
      (printf "\n# Remove dirs that we created but not own only if empty\n")
      (printf "while true; do read R || break; ~a"
              "rmdir -- \"$R\" > /dev/null 2>&1; done \\\n")
      (printf "<<::://E//O//F////T//W//O//:::\n")
      (for ([p path-changes] #:when (eq? 'md (car p)))
        (printf "~a\n" (cadr p)))
      (printf "::://E//O//F////T//W//O//:::\n")
      (printf "\n# Remove this script\n")
      (printf "exec rm \"$0\"\n")))
  (run "chmod" "+x" uninstaller))

;; we need a namespace to compile the new config, grab it now, before the
;; collection tree moves (otherwise it won't find the `scheme' collection)
(define base-ns (make-base-namespace))

(define write-config
  (case-lambda
    [()  (write-config #t (dir: 'collects))]
    [(x) (if (boolean? x)
           (write-config x (dir: 'collects))
           (write-config #t x))]
    [(compile? collectsdir)
     (define (cpath . xs)
       (apply make-path collectsdir "config" xs))
     (define (ftime file)
       (and (file-exists? file) (file-or-directory-modify-seconds file)))
     (let* ([src (cpath "config.rkt")]
            [zo  (cpath "compiled" "config_rkt.zo")]
            ;; [dep (cpath "compiled" "config_rkt.dep")] ; not needed
            [src-time (ftime src)]
            [zo-time  (ftime zo)])
       (printf "Rewriting configuration file at: ~a...\n" src)
       (parameterize ([current-namespace base-ns] ; to compile (see above)
                      [current-library-collection-paths ; for configtab.rkt
                       (list collectsdir)])
         (with-output-to-file src #:exists 'truncate/replace
           (lambda ()
             (printf ";; automatically generated by unixstyle-install\n")
             (printf "(module config setup/configtab\n")
             (printf "  (define doc-dir ~s)\n" (dir: 'doc))
             (when (eq? 'shared (system-type 'link)) ; never true for now
               (printf "  (define dll-dir ~s)\n" (dir: 'lib)))
             (printf "  (define lib-dir ~s)\n" (dir: 'librkt))
             (printf "  (define include-dir ~s)\n" (dir: 'includerkt))
             (printf "  (define bin-dir ~s)\n" (dir: 'bin))
             (printf "  (define absolute-installation? #t))\n")))
         ;; recompile & set times as if nothing happened (don't remove .dep)
         ;; this requires the file to look the same on all compilations, and
         ;; configtab.rkt generates bindings unhygienically for that reason.
         (when compile?
           (when src-time (file-or-directory-modify-seconds src src-time))
           (if (not zo-time)
             (printf "WARNING: skipping recompilation, no zo file at ~a\n" zo)
             (begin
               (printf "Recompiling to ~a...\n" zo)
               (with-input-from-file src
                 (lambda ()
                   (with-output-to-file zo #:exists 'truncate/replace
                     (lambda () (write (compile (read-syntax)))))))
               (file-or-directory-modify-seconds zo zo-time))))))]))

;; creates a directory including its ancestors when needed
(define (make-dir* dir)
  (unless (directory-exists? dir)
    (make-dir* (dirname dir))
    (make-directory dir)
    (register-change! 'md dir)))

(define yes-to-all? #f)
(define (ask-overwrite kind path)
  (let ([rm (lambda () (rm path))])
    (if yes-to-all?
      (rm)
      (begin (printf "Overwrite ~a \"~a\"?\n" kind path)
             (let loop ()
               (printf "  [y]es / yes to [a]ll / [n]o=abort > ")
               (case (cond [(regexp-match #rx"^[ \t]*([a-z])"
                                          (string-downcase (read-line)))
                            => (lambda (m) (string->symbol (cadr m)))]
                           [else #f])
                 [(y) (rm)]
                 [(a) (set! yes-to-all? #t) (rm)]
                 [(n) (error "Abort!")]
                 [else (loop)]))))))

(define ((move/copy-tree move?) src dst* #:missing [missing 'error])
  (define dst (if (symbol? dst*) (dir: dst*) dst*))
  (define src-exists?
    (or (directory-exists? src) (file-exists? src) (link-exists? src)))
  (printf "~aing ~a -> ~a\n" (if move? "Mov" "Copy") src dst)
  (cond
    [src-exists?
     (make-dir* (dirname dst))
     (let loop ([src (path->string (simplify-path src #f))]
                [dst (path->string (simplify-path dst #f))]
                [lvl (level-of src)]) ; see above
       (let ([doit (let ([doit (if move? mv* cp*)]) (lambda () (doit src dst)))]
             [src-d? (directory-exists? src)]
             [dst-l? (link-exists? dst)]
             [dst-d? (directory-exists? dst)]
             [dst-f? (file-exists? dst)])
         (unless (skip-filter src)
           (when (and src-d? (not lvl) (not dst-d?))
             (when (or dst-l? dst-f?) (ask-overwrite "file or link" dst))
             (make-directory dst)
             (register-change! 'md dst)
             (set! dst-d? #t) (set! dst-l? #f) (set! dst-f? #f))
           (cond [dst-l? (ask-overwrite "symlink" dst) (doit)]
                 [dst-d? (if (and src-d? (or (not lvl) (< 0 lvl)))
                           ;; recurse only when source is dir, & not too deep
                           (for-each (lambda (name)
                                       (loop (make-path src name)
                                             (make-path dst name)
                                             (and lvl (sub1 lvl))))
                                     (ls src))
                           (begin (ask-overwrite "dir" dst) (doit)))]
                 [dst-f? (ask-overwrite "file" dst) (doit)]
                 [else (doit)]))))
     (when move? (remove-empty-dirs src))]
    [(eq? missing 'error)
     (error (format "  missing source path ~s, aborting..." src))]
    [(eq? missing 'skip)
     (printf "  missing source path ~s, skipping...\n" src)]
    [else (error 'move/copy-tree "internal error, unknown mode: ~e" missing)]))

;; --------------------------------------------------------------------------

(define (move/copy-distribution move?)
  (define do-tree (move/copy-tree move?))
  (current-directory rktdir)
  (when (ormap (lambda (p) (regexp-match #rx"libracket.*[.]so" p)) (ls "lib"))
    (error "Cannot handle distribution of shared-libraries (yet)"))
  (with-handlers ([exn? (lambda (e) (undo-changes) (raise e))])
    (define binfiles (ls "bin")) ; see below
    (do-tree "bin"      'bin)
    (do-tree "collects" 'collects)
    (do-tree "doc"      'doc #:missing 'skip) ; not included in text distros
    ;; (do-tree ??? 'lib) ; shared stuff goes here
    (do-tree "include"  'includerkt)
    (do-tree "lib"      'librkt)
    (do-tree "man"      'man)
    ;; (when (and (not (equal? (dir: 'src) "")) (directory-exists? "src"))
    ;;   (do-tree "src" 'src))
    ;; don't use the above -- it would be pointless to put the source tree in
    ;; a place where it would not be usable.
    (when (and (directory-exists? "src") move?) (rm "src"))
    ;; part of the distribution:
    (when (file-exists? "README")
      (do-tree "README" (make-path (dir: 'doc) "README")))
    ;; nothing should be left now if this was a move
    (when (and move? (not (null? (ls))))
      (error (format "leftovers in source tree: ~s" (ls))))
    ;; we need to know which files need fixing
    (fix-executables (dir: 'bin) (dir: 'librkt) binfiles)
    (write-uninstaller)
    (write-config))
  (when move?
    (current-directory (dirname rktdir))
    (delete-directory rktdir)))

(define (make-install-copytree)
  (define copytree (move/copy-tree #f))
  (define origtree? (equal? "yes" (get-arg)))
  (current-directory rktdir)
  (set! skip-filter ; skip all dot-names and compiled subdirs
        (lambda (p) (regexp-match? #rx"^(?:[.].*|compiled)$" (basename p))))
  (with-handlers ([exn? (lambda (e) (undo-changes) (raise e))])
    (set! yes-to-all? #t) ; non-interactive
    (copytree "collects" 'collects)
    (copytree "doc"      'doc)
    (copytree "man"      'man)
    (unless origtree? (write-config #f)))) ; don't recompile

(define (make-install-destdir-fix)
  (define destdir
    (or (getenv "DESTDIR")
        (error "missing DESTDIR value for make-install-destdir-fix")))
  (define destdirlen (string-length destdir))
  (define origtree? (equal? "yes" (get-arg)))
  ;; grab paths before we change them
  (define bindir      (dir: 'bin))
  (define librktdir   (dir: 'librkt))
  (define collectsdir (dir: 'collects))
  (define (remove-dest p)
    (let ([pfx (and (< destdirlen (string-length p))
                    (substring p 0 destdirlen))])
      (if (equal? pfx destdir)
        (regexp-replace #rx"^/*" (substring p destdirlen) "/")
        (error (format "expecting a DESTDIR prefix of ~s in ~s" destdir p)))))
  (set! dirs (map (lambda (d) (list (car d) (remove-dest (cadr d)))) dirs))
  ;; no need to send an explicit binfiles argument -- this function is used
  ;; only when DESTDIR is present, so we're installing to a directory that
  ;; has only our binaries
  (fix-executables bindir librktdir)
  (unless origtree? (write-config collectsdir)))

;; --------------------------------------------------------------------------

(module+ main
  (case op
    [(move) (move/copy-distribution #t)]
    [(copy) (move/copy-distribution #f)]
    [(make-install-copytree)    (make-install-copytree)]
    [(make-install-destdir-fix) (make-install-destdir-fix)]
    [else   (error (format "unknown operation: ~e" op))]))
