;; This file is used to move the PLT tree as part of a Unix sh-installer (when
;; it works in unix-style mode).  This is done carefully (undoing changes if
;; there is an error), and a plt-uninstall script is generated.  There is no
;; good cmdline interface, since it is internal, and should be as independent
;; as possible (it moves the collection tree).
;; Expects these arguments:
;; * The source plt directory
;; * Path names that should be copied (bin, collects, doc, lib, ...)
;; >>> plt/src/copytree.ss should be merged into this
(module unixstyle-install mzscheme

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
  ;; (define srcdir        (get-arg))

  ;; Configures level where we start owning stuff (in the sense that the
  ;; generated uninstaller will remove it, and the installation will remove
  ;; dirs instead of moving into them).  For example, a 1-level for collectsdir
  ;; means that we will overwrite collects/foo instead of adding files in it.
  ;; A level of 999 means that we always add stuff if the directory exists; a
  ;; level of 0 means that we always own the directory (should never be used).
  ;; In any case, we start own stuff (= add it to the uninstaller) once we
  ;; encounter a directory that does not already exist.  #f means that we never
  ;; own directories, only files.
  (define (level-of dir)
    (cond [(equal? dir "bin")      #f]
          [(equal? dir "collects") 1]
          [(equal? dir "doc")      1]
          [(equal? dir "include")  1]
          ;; if shared libraries are used, then these files should be moved
          ;; independently, as if they had a level of #f
          [(equal? dir "lib")      1]
          [(equal? dir "man")      #f]
          [(equal? dir "src")      1]
          [(equal? dir "readme.txt") #f] ; moved last
          [else (error 'level-of "internal-error: unknown dir ~e" dir)]))

  (define (->string x)
    (if (path? x) (path->string x) x))

  (define (dirname path)
    (let-values ([(base name dir?) (split-path path)]) base))

  ;; convenient wrapper for a simple subprocess
  (define (run cmd . args)
    (let-values
        ([(p _1 _2 _3)
          (apply subprocess
                 (current-output-port) (current-input-port) (current-error-port)
                 (find-executable-path cmd) (map ->string args))])
      (subprocess-wait p)
      (unless (zero? (subprocess-status p))
        (error (format "~a: returned an error exit code"
                       (let ([s (format "~a" (cons cmd args))])
                         (substring s 1 (sub1 (string-length s)))))))))

  ;; removes a file or a directory (recursively)
  (define (rm path)
    (cond [(or (file-exists? path) (link-exists? path)) (delete-file path)]
          [(directory-exists? path)
           (parameterize ([current-directory path])
             (for-each rm (directory-list)))
           (delete-directory path)]
          [else #t])) ; shouldn't happen

  ;; copy a file or a directory (recursively), preserving time stamps
  ;; (mzscheme's copy-file preservs permission bits)
  #; ; this is impossible now: there is no way to create arbitrary symlink
  (define (cp src dst)
    ...)

  ;; try to rename and if it fails (due to different fs) copy and remove
  (define (mv src dst)
    (unless (with-handlers ([(lambda (e)
                               (and (exn:fail:filesystem? e)
                                    (not (exn:fail:filesystem:exists? e))))
                             (lambda (e) #f)])
              (rename-file-or-directory src dst) #t)
      ;; move failed: copy & remove
      (with-handlers ([void (lambda (e)
                              ;; error => remove new copy (if can) and re-raise
                              (with-handlers ([void (lambda (e) #f)])
                                (rm dst)
                                (raise e)))])
        ;; (cp src dst) (rm src)
        ;; can't do it in Scheme, run /bin/mv instead
        (run "mv" "--" src dst))))

  ;; list of changes, so we can undo them in case of an error and so we can
  ;; create an uninstaller -- a pair is for a move, and a string/path is for a
  ;; removed directory
  (define path-changes '())

  ;; like `mv', but also record moves
  (define (mv* src dst)
    (mv src dst)
    (set! path-changes (cons (cons src dst) path-changes)))

  (define binary-re
    #rx#"coLLECTs dIRECTORy:")
  (define script-re
    (byte-regexp (bytes-append #"\n# Make this PATH-independent\n.*?"
                               #"\nbindir=\"[$]D\"\nPATH=\"[$]saveP\"\n")))

  (define (bin-mover src dst)
    (define (binary-move)
      ;; don't move => modify a copy of the running mzscheme
      (copy-file src dst) (delete-file src)
      (let-values ([(i o) (open-input-output-file dst 'update)])
        (let ([m (regexp-match-positions binary-re i)])
          (unless m
            (error
             (format "could not find collection-path label in executable: ~a"
                     src)))
          (file-position o (cdar m))
          (display collectsdir o)
          (write-byte 0 o)
          (write-byte 0 o)
          (close-input-port i)
          (close-output-port o))))
    (define (script-move)
      (let* ([size (file-size src)]
             [buf (with-input-from-file src (lambda () (read-bytes size)))]
             [m (or (regexp-match-positions script-re buf)
                    (error (format "could not find binpath block in script: ~a"
                                   src)))])
        (with-output-to-file dst
          (lambda ()
            (write-bytes buf (current-output-port) 0 (caar m))
            (printf "\nbindir=\"~a\"\n"
                    (regexp-replace* #rx"[\"`'$\\]" (->string bindir) "\\\\&"))
            (write-bytes buf (current-output-port) (cdar m)))
          'truncate/replace)
        (delete-file src)))
    (let ([magic (with-input-from-file src (lambda () (read-bytes 10)))])
      (cond [(regexp-match #rx#"^\177ELF" magic) (binary-move)]
            [(regexp-match #rx#"^#!/bin/sh" magic) (script-move)]
            [else (error (format "unknown binary type: ~a" src))])
      ;; undo might get back modified files, but the installer will remove them
      (set! path-changes (cons (cons src dst) path-changes))
      (run "chmod" "+x" dst)))

  ;; remove and record all empty dirs
  (define (remove-empty-dirs dir)
    (let loop ([dir dir] [recurse? #t])
      (when (and (directory-exists? dir) (not (link-exists? dir)))
        (let ([ps (directory-list dir)])
          (cond [(null? ps)
                 (delete-directory dir)
                 (set! path-changes (cons dir path-changes))]
                [recurse?
                 (for-each (lambda (p) (loop (build-path dir p) #t)) ps)
                 (loop dir #f)] ; try again
                ;; get here only on the 2nd round, so we cannot remove it
                )))))

  ;; called from an error handler, so avoid raising more errors
  (define (undo-changes)
    (printf "...undoing changes\n")
    (for-each (lambda (p)
                (if (pair? p)
                  (with-handlers ([exn?
                                   (lambda (e)
                                     (fprintf (current-error-port)
                                              "  ** error during undo: ~a\n"
                                              (exn-message e))
                                     #f)])
                    (mv (cdr p) (car p)))
                  (make-directory p)))
              path-changes))

  (define (write-uninstaller)
    (define uninstaller (build-path bindir "plt-uninstall"))
    (with-output-to-file uninstaller
      (lambda ()
        (printf "#!/bin/sh\n")
        (printf "while true; do read R || break; rm -rf -- \"$R\"; done \\\n")
        (printf "<<::://E//O//F//:::\n")
        (for-each (lambda (p) (when (pair? p) (printf "~a\n" (cdr p))))
                  path-changes)
        (printf "::://E//O//F//:::\n")
        (printf "exec rm \"$0\"\n"))
      'replace)
    (run "chmod" "+x" uninstaller))

  (define (write-config)
    (define (cpath . xs) (apply build-path collectsdir "config" xs))
    (with-output-to-file (cpath "config.ss")
      (lambda ()
        (printf ";; automatically generated at installation\n")
        (printf "(module config (lib \"configtab.ss\" \"setup\")\n")
        (printf "  (define doc-dir ~s)\n" docdir)
        (when (eq? 'shared (system-type 'link)) ; never true for now
          (printf "  (define dll-dir ~s)\n" libdir))
        (printf "  (define lib-dir ~s)\n" libpltdir)
        (printf "  (define include-dir ~s)\n" includepltdir)
        (printf "  (define bin-dir ~s)\n" bindir)
        (printf "  (define absolute-installation? #t))\n"))
      'truncate/replace)
    ;; in case the system time is broken:
    (rm (cpath "compiled" "config.zo"))
    (rm (cpath "compiled" "config.dep")))

  ;; creates a directory including its ancestors when needed
  (define (make-dir* dir)
    (unless (directory-exists? dir)
      (make-dir* (dirname dir))
      (make-directory dir)))

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

  (define (move-tree src dst . mover)
    (printf "Moving ~a -> ~a\n" src dst)
    (make-dir* (dirname dst))
    (let loop ([src (simplify-path src #f)]
               [dst (simplify-path dst #f)]
               [lvl (level-of src)]) ; see above
      (let ([mv (let ([mv (if (pair? mover) (car mover) mv*)])
                  (lambda () (mv src dst)))]
            [src-d? (directory-exists? src)]
            [dst-l? (link-exists? dst)]
            [dst-d? (directory-exists? dst)]
            [dst-f? (file-exists? dst)])
        (when (and src-d? (not lvl) (not dst-d?))
          (when (or dst-l? dst-f?) (ask-overwrite "file or link" dst))
          (make-directory dst)
          (set! dst-d? #t) (set! dst-l? #f) (set! dst-f? #f))
        (cond [dst-l? (ask-overwrite "symlink" dst) (mv)]
              [dst-d? (if (and src-d? (or (not lvl) (< 0 lvl)))
                        ;; recurse only when the source is a dir, & not too deep
                        (for-each (lambda (name)
                                    (loop (build-path src name)
                                          (build-path dst name)
                                          (and lvl (sub1 lvl))))
                                  (directory-list src))
                        (begin (ask-overwrite "dir" dst) (mv)))]
              [dst-f? (ask-overwrite "file" dst) (mv)]
              [else (mv)])))
    (remove-empty-dirs src))

  ;; --------------------------------------------------------------------------

  (current-directory pltdir)

  (when (ormap (lambda (p) (regexp-match #rx"[.]so" (->string p)))
               (directory-list "lib"))
    (error "Cannot handle distribution of shared-libraries (yet)"))

  (with-handlers ([void (lambda (e) (undo-changes) (raise e))])
    (move-tree "bin"      bindir bin-mover)
    (move-tree "collects" collectsdir)
    (move-tree "doc"      docdir)
    ;; (move-tree libdir) ; shared stuff goes here
    (move-tree "include"  includepltdir)
    (move-tree "lib"      libpltdir)
    (move-tree "man"      mandir)
    ;; (when (and (not (equal? srcdir "")) (directory-exists? "src"))
    ;;   (move-tree "src" srcdir))
    ;; don't use the above -- it would be pointless to put the source tree in a
    ;; place where it would not be usable.
    (when (directory-exists? "src") (rm "src"))
    ;; part of the distribution:
    (move-tree "readme.txt" (build-path docdir "readme.txt"))
    ;; nothing should be left now
    (let ([ps (map ->string (directory-list))])
      (unless (null? ps)
        (error (format "Error: leftovers in source tree: ~s" ps))))
    (write-uninstaller)
    (write-config))
  (current-directory (dirname pltdir))
  (delete-directory pltdir)

  )
