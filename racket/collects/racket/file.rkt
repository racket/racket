#lang racket/base
(require "path.rkt"
         setup/dirs
         (for-syntax racket/base
                     setup/path-to-relative))

(provide delete-directory/files
         copy-directory/files
         make-directory*
         make-temporary-file

         get-preference
         put-preferences
         preferences-lock-file-mode
         make-handle-get-preference-locked
         make-lock-file-name
         call-with-file-lock/timeout

         call-with-atomic-output-file

         fold-files
         find-files
         pathlist-closure

         file->string
         file->bytes
         file->value
         file->lines
         file->bytes-lines
         file->list
         display-to-file
         write-to-file
         display-lines-to-file

         user-read-bit
         user-write-bit
         user-execute-bit
         group-read-bit
         group-write-bit
         group-execute-bit
         other-read-bit
         other-write-bit
         other-execute-bit)

(require "private/portlines.rkt")

;; utility: sorted dirlist so functions are deterministic
(define (sorted-dirlist [dir (current-directory)])
  (let* ([ps (directory-list dir)]
         [ps (map (lambda (p) (cons (path->string p) p)) ps)]
         [ps (sort ps (lambda (p1 p2) (string<? (car p1) (car p2))))]
         [ps (map cdr ps)])
    ps))

(define (delete-directory/files path
                                #:must-exist? [must-exist? #t])
  (unless (path-string? path)
    (raise-argument-error 'delete-directory/files "path-string?" path))
  (let loop ([path path])
    (cond
     [(or (link-exists? path) (file-exists? path))
      (delete-file path)]
     [(directory-exists? path)
      (for-each (lambda (e) (loop (build-path path e)))
                (sorted-dirlist path))
      (delete-directory path)]
     [else
      (when must-exist?
        (raise-not-a-file-or-directory 'delete-directory/files path))])))

(define (raise-not-a-file-or-directory who path)
  (raise
   (make-exn:fail:filesystem
    (format "~a: encountered ~a, neither a file nor a directory"
            who
            path)
    (current-continuation-marks))))

(define (copy-directory/files src dest 
                              #:keep-modify-seconds? [keep-modify-seconds? #f])
  (let loop ([src src] [dest dest])
    (cond [(file-exists? src)
           (copy-file src dest)
           (when keep-modify-seconds?
             (file-or-directory-modify-seconds
              dest
              (file-or-directory-modify-seconds src)))]
          [(directory-exists? src)
           (make-directory dest)
           (for-each (lambda (e)
                       (loop (build-path src e)
                             (build-path dest e)))
                     (sorted-dirlist src))]
          [else (raise-not-a-file-or-directory 'copy-directory/files src)])))

(define (make-directory* dir)
  (let-values ([(base name dir?) (split-path dir)])
    (when (and (path? base)
               (not (directory-exists? base)))
      (make-directory* base))
    (unless (directory-exists? dir)
      (with-handlers ([exn:fail:filesystem:exists? void])
        (make-directory dir)))))

(define-syntax (make-temporary-file stx)
  (with-syntax ([app (datum->syntax stx #'#%app stx)])
    (syntax-case stx ()
      [x (identifier? #'x) #'make-temporary-file/proc]
      [(_)
       (let ()
         (define line   (syntax-line stx))
         (define col    (syntax-column stx))
         (define source (syntax-source stx))
         (define pos    (syntax-position stx))
         (define str-src
           (cond [(path? source)
                  (regexp-replace #rx"^<(.*?)>(?=/)"
                                  (path->relative-string/library source)
                                  (lambda (_ s) (string-upcase s)))]
                 [(string? source) source]
                 [else #f]))
         (define str-loc
           (cond [(and line col) (format "-~a-~a" line col)]
                 [pos (format "--~a" pos)]
                 [else ""]))
         (define combined-str (string-append (or str-src "rkttmp") str-loc))
         (define sanitized-str (regexp-replace* #rx"[<>:\"/\\|]" combined-str "-"))
         (define max-len 50) ;; must be even
         (define not-too-long-str
           (cond [(< max-len (string-length sanitized-str))
                  (string-append (substring sanitized-str 0 (- (/ max-len 2) 2))
                                 "----"
                                 (substring sanitized-str
                                            (- (string-length sanitized-str)
                                               (- (/ max-len 2) 2))))]
                 [else sanitized-str]))
         #`(app make-temporary-file/proc
                #,(string-append not-too-long-str "_~a")))]
      [(_ . whatever)
       #'(app make-temporary-file/proc . whatever)])))

(define make-temporary-file/proc
  (let ()
    (define (make-temporary-file [template "rkttmp~a"] [copy-from #f] [base-dir #f])
      (with-handlers ([exn:fail:contract?
                       (lambda (x)
                         (raise-arguments-error 'make-temporary-file
                                                "format string does not expect 1 argument"
                                                "format string" template))])
        (format template void))
      (unless (or (not copy-from)
                  (path-string? copy-from)
                  (eq? copy-from 'directory))
        (raise-argument-error 'make-temporary-file
                              "(or/c path-string? 'directory #f)"
                              copy-from))
      (unless (or (not base-dir) (path-string? base-dir))
        (raise-argument-error 'make-temporary-file
                              "(or/c path-string? #f)"
                              base-dir))
      (let ([tmpdir (find-system-path 'temp-dir)])
        (let loop ([s (current-seconds)]
                   [ms (inexact->exact (truncate (current-inexact-milliseconds)))])
          (let ([name (let ([n (format template (format "~a~a" s ms))])
                        (cond [base-dir (build-path base-dir n)]
                              [(relative-path? n) (build-path tmpdir n)]
                              [else n]))])
            (with-handlers ([exn:fail:filesystem:exists?
                             (lambda (x)
                               ;; try again with a new name
                               (loop (- s (random 10))
                                     (+ ms (random 10))))])
              (if copy-from
                  (if (eq? copy-from 'directory)
                      (make-directory name)
                      (copy-file copy-from name))
                  (close-output-port (open-output-file name)))
              name)))))
    make-temporary-file))

;;  Open a temporary path for writing, automatically renames after,
;;  and arranges to delete path if there's an exception. Uses the an
;;  extra rename dance as needed under Windows to ensure that any
;;  existing readers of the file do not prevent updating the
;;  file. Breaks are managed so that the port is reliably closed and
;;  the file is reliably deleted if there's a break.
(define (call-with-atomic-output-file path 
                                      proc
                                      #:security-guard [guard #f])
  (unless (path-string? path)
    (raise-argument-error 'call-with-atomic-output-file "path-string?" path))
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 2))
    (raise-argument-error 'call-with-atomic-output-file "(procedure-arity-includes/c 2)" proc))
  (unless (or (not guard)
              (security-guard? guard))
    (raise-argument-error 'call-with-atomic-output-file "(or/c #f security-guard?)" guard))
  (define (try-delete-file path [noisy? #t])
    ;; Attempt to delete, but give up if it doesn't work:
    (with-handlers ([exn:fail:filesystem? void])
      (delete-file path)))
  (let ([bp (current-break-parameterization)]
        [tmp-path (parameterize ([current-security-guard (or guard (current-security-guard))])
                    (make-temporary-file "tmp~a" #f (path-only path)))]
        [ok? #f])
    (dynamic-wind
     void
     (lambda ()
       (begin0
         (let ([out (parameterize ([current-security-guard (or guard (current-security-guard))])
                      (open-output-file tmp-path #:exists 'truncate/replace))])
           (dynamic-wind
            void
            (lambda ()
              (call-with-break-parameterization bp (lambda () (proc out tmp-path))))
            (lambda ()
              (close-output-port out))))
         (set! ok? #t)))
     (lambda ()
       (parameterize ([current-security-guard (or guard (current-security-guard))])
         (if ok?
             (if (eq? (system-type) 'windows)
                 (let ([tmp-path2 (make-temporary-file "tmp~a" #f (path-only path))])
                   (with-handlers ([exn:fail:filesystem? void])
                     (rename-file-or-directory path tmp-path2 #t))
                   (rename-file-or-directory tmp-path path #t)
                   (try-delete-file tmp-path2))
                 (rename-file-or-directory tmp-path path #t))
             (try-delete-file tmp-path)))))))

(define (with-pref-params thunk)
  (parameterize ([read-case-sensitive #f]
                 [read-square-bracket-as-paren #t]
                 [read-curly-brace-as-paren #t]
                 [read-accept-box #t]
                 [read-accept-compiled #f]
                 [read-accept-bar-quote #t]
                 [read-accept-graph #t]
                 [read-decimal-as-inexact #t]
                 [read-accept-dot #t]
                 [read-accept-infix-dot #t]
                 [read-accept-quasiquote #t]
                 [read-accept-reader #f]
                 [print-struct #f]
                 [print-graph #f] ; <--- FIXME: temporary solution to DrRacket-pref problem
                 [print-box #t]
                 [print-vector-length #t]
                 [current-readtable #f])
    (thunk)))

(define pref-cache (make-weak-box #f))

(define (path->key p)
  (string->symbol (bytes->string/latin-1 (path->bytes p))))

(define (pref-cache-install! fn-key fn-date f)
  (let ([table (or (weak-box-value pref-cache)
                   (make-hasheq))])
    (hash-set! table 
               (path->key fn-key)
               (cons
                (file-or-directory-modify-seconds fn-date #f (lambda () -inf.0))
                f))
    (unless (eq? table (weak-box-value pref-cache))
      (set! pref-cache (make-weak-box table)))))

(define (make-pathless-lock-file-name name)
  (bytes->path-element
   (bytes-append
    (if (eq? 'windows (system-type))
        #"_"
        #".")
    #"LOCK"
    (path-element->bytes name))))

(define make-lock-file-name
  (case-lambda
   [(path)
    (unless (path-string? path)
      (raise-argument-error 'make-lock-file-name "path-string?" path))
    (let-values ([(dir name dir?) (split-path path)])
      (if (eq? dir 'relative)
          (make-pathless-lock-file-name name)
          (make-lock-file-name dir name)))]
   [(dir name)
    (unless (path-string? dir)
      (raise-argument-error 'make-lock-file-name "path-string?" dir))
    (unless (path-element? name)
      (raise-argument-error 'make-lock-file-name "path-element?" name))
    (build-path dir
                (make-pathless-lock-file-name name))]))

(define (preferences-lock-file-mode)
  (case (system-type)
    [(windows) 'file-lock]
    [else 'exists]))

(define (call-with-file-lock/timeout fn kind thunk failure-thunk
                                     #:lock-file [lock-file #f]
                                     #:delay [delay 0.01]
                                     #:max-delay [max-delay 0.2])
  
  (unless (or (path-string? fn) (eq? fn #f))
    (raise-argument-error 'call-with-file-lock/timeout "(or/c path-string? #f)" fn))
  (unless (or (eq? kind 'shared) (eq? kind 'exclusive))
    (raise-argument-error 'call-with-file-lock/timeout "(or/c 'shared 'exclusive)" kind))
  (unless (and (procedure? thunk) (procedure-arity-includes? thunk 0))
    (raise-argument-error 'call-with-file-lock/timeout "(-> any)" thunk))
  (unless (and (procedure? thunk) (procedure-arity-includes? failure-thunk 0))
    (raise-argument-error 'call-with-file-lock/timeout "(-> any)" failure-thunk))
  (unless (or (not lock-file) (path-string? lock-file))
    (raise-argument-error 'call-with-file-lock/timeout "(or/c path-string? #f)" lock-file))
  (unless (and (real? delay) (not (negative? delay)))
    (raise-argument-error 'call-with-file-lock/timeout "(>=/c 0.0)" delay))
  (unless (and (real? max-delay) (not (negative? max-delay)))
    (raise-argument-error 'call-with-file-lock/timeout "(>=/c 0.0)" max-delay))
  
  (define real-lock-file (or lock-file (make-lock-file-name fn)))
  (let loop ([delay delay])
    (call-with-file-lock 
     kind 
     real-lock-file
     thunk
     (lambda ()
       (if (delay . < . max-delay)
           (begin
             (sleep delay)
             (loop (* 2 delay)))
           (failure-thunk))))))
    
(define (call-with-preference-file-lock who kind get-lock-file thunk lock-there)
  (define lock-style (preferences-lock-file-mode))
  (define lock-file (get-lock-file))
  (define failure-thunk 
    (if lock-there 
        (lambda () (lock-there lock-file))
        (lambda ()
          (case lock-style
            [(file-lock) (error who
                                "~a ~a: ~e"
                                "some other process has a lock"
                                "on the preferences lock file"
                                lock-file)]
            [else (error who
                         "~a, ~a: ~e"
                         "some other process has the preference-file lock"
                         "as indicated by the existence of the lock file"
                         lock-file)]))))
                                  
  (call-with-file-lock kind lock-file thunk failure-thunk #:lock-style lock-style))

(define (call-with-file-lock kind lock-file thunk failure-thunk #:lock-style [lock-style 'file-lock])
  (case lock-style
    [(file-lock)
      ;; Create the lock file if it doesn't exist:
      (unless (file-exists? lock-file)
        (with-handlers ([exn:fail:filesystem:exists? (lambda (exn) 'ok)])
          (close-output-port (open-output-file lock-file #:exists 'error))))
      (((if (eq? kind 'exclusive)
            (lambda (fn proc) (call-with-output-file* fn proc #:exists 'update))
            call-with-input-file*)
        lock-file
        (lambda (p)
          (if (port-try-file-lock? p kind)
              ;; got lock:
              (call-with-values
               (lambda ()
                 (dynamic-wind
                     void
                     thunk
                     (lambda ()
                       (port-file-unlock p))))
               (lambda vs (lambda () (apply values vs))))
              ;; didn't get lock:
              (lambda () (failure-thunk))))))]
    [else ; = 'exists
     ;; Only a write lock is needed, and the file lock 
     ;; is implemented by the presence of the file:
     (case kind
       [(shared) (thunk)]
       [(exclusive)
        (with-handlers ([exn:fail:filesystem:exists? 
                         (lambda (x) (failure-thunk))])
          ;; Grab lock:
          (close-output-port (open-output-file lock-file #:exists 'error)))
        (dynamic-wind 
            void
            thunk
            (lambda ()
              ;; Release lock:
              (delete-file lock-file)))])]))

(define (get-prefs flush-mode filename use-lock? lock-there)
  (define (read-prefs default-pref-file)
    (with-handlers ([exn:fail:filesystem? (lambda (x) null)])
      (let-values ([(pref-file use-lock?)
                    (if filename
                        (values filename use-lock?)
                        (let ([f default-pref-file])
                          (if (file-exists? f)
                              ;; Using `file-exists?' means there's technically a
                              ;; race condition, but something has gone really wrong
                              ;; if the file disappears.
                              (values f use-lock?)
                              ;; Look for old PLT Scheme pref file:
                              (let ([alt-f 
                                     (case (system-type)
                                       [(windows)
                                        (build-path (find-system-path 'pref-dir)
                                                    'up "PLT Scheme" "plt-prefs.ss")]
                                       [(macosx)
                                        (build-path (find-system-path 'pref-dir)
                                                    "org.plt-scheme.prefs.ss")]
                                       [(unix)
                                        (expand-user-path "~/.plt-scheme/plt-prefs.ss")])])
                                (if (file-exists? alt-f)
                                    (values alt-f #f)
                                    ;; Last chance: check for a "racket-prefs.rtkd" file
                                    ;; in the configuration directory:
                                    (values
                                     (let* ([d (find-config-dir)]
                                            [c-f (and d (build-path d "racket-prefs.rktd"))])
                                       (if (and c-f (file-exists? c-f))
                                           c-f
                                           ;; Trigger a filesystem error:
                                           (call-with-input-file* f void)))
                                     #f))))))])
        (let ([prefs (with-pref-params
                      (lambda ()
                        (with-handlers ([exn:fail:read? (lambda (exn)
                                                          (log-error 
                                                           (format "error reading preferences: ~a"
                                                                   (exn-message exn)))
                                                          null)])
                          (if use-lock? 
                              (call-with-preference-file-lock
                               'get-preference
                               'shared
                               (lambda ()
                                 (make-lock-file-name pref-file))
                               (lambda ()
                                 (with-input-from-file pref-file read))
                               lock-there)
                              (with-input-from-file pref-file read)))))])
          ;; Make sure file content had the right shape:
          (if (and (list? prefs)
                   (andmap (lambda (x)
                             (and (pair? x) 
                                  (symbol? (car x))
                                  (pair? (cdr x)) 
                                  (null? (cddr x))))
                           prefs))
              prefs
              (begin
                (log-error "preference file content is not a list of symbol--value lists")
                null))))))
  (let* ([fn (path->complete-path
              (or filename
                  (find-system-path 'pref-file)))]
         [cache (let ([table (weak-box-value pref-cache)])
                  (and table (hash-ref table (path->key fn) #f)))])
    (if (and cache
             (or (not flush-mode)
                 (and (eq? flush-mode 'timestamp)
                      (= (car cache)
                         (file-or-directory-modify-seconds fn #f (lambda () -inf.0))))))
        (cdr cache)
        (let ([ts (file-or-directory-modify-seconds fn  #f (lambda () -inf.0))]
              [f (read-prefs fn)])
          (pref-cache-install! fn fn f)
          f))))

(define (make-handle-get-preference-locked delay 
                                           name 
                                           [fail-thunk (lambda () #f)]
                                           [refresh-cache? 'timestamp]
                                           [filename #f]
                                           #:lock-there [lock-there #f]
                                           #:max-delay [max-delay 0.2])
  (lambda (lock-filename)
    (sleep delay)
    (get-preference name fail-thunk refresh-cache? filename 
                    #:lock-there (let ([new-delay (* 2 delay)])
                                   (if (new-delay . < . max-delay)
                                       (make-handle-get-preference-locked
                                        new-delay
                                        name fail-thunk refresh-cache? filename
                                         #:lock-there lock-there
                                         #:max-delay max-delay)
                                       lock-there)))))

(define (get-preference name [fail-thunk (lambda () #f)]
                        [refresh-cache? 'timestamp]
                        [filename #f]
                        #:timeout-lock-there [timeout-lock-there #f]
                        #:lock-there [lock-there 
                                      (make-handle-get-preference-locked
                                       0.01
                                       name
                                       fail-thunk
                                       refresh-cache?
                                       filename
                                       #:lock-there timeout-lock-there)]
                        #:use-lock? [use-lock? #t])
  (unless (symbol? name)
    (raise-argument-error 'get-preference "symbol?" name))
  (unless (and (procedure? fail-thunk)
               (procedure-arity-includes? fail-thunk 0))
    (raise-argument-error 'get-preference "(-> any)" fail-thunk))
  ((let/ec esc
     (let ([f (get-prefs refresh-cache? filename use-lock? 
                         (and lock-there
                              (lambda (file)
                                (esc (lambda () (lock-there file))))))])
       (lambda ()
         (let ([m (assq name f)])
           (if m (cadr m) (fail-thunk))))))))

(define (put-preferences names vals [lock-there #f] [filename #f])
  (unless (and (list? names) (andmap symbol? names))
    (raise-argument-error 'put-preferences "(listof symbol?)" names))
  (unless (list? vals)
    (raise-argument-error 'put-preferences "list?" vals))
  (unless (= (length names) (length vals))
    (raise-arguments-error
     'put-preferences
     "the length of the name list does not match the length of the value list"
     "name list length" (length names) 
     "value list length" (length vals)
     "name list" names
     "value list" vals))
  (let-values ([(pref-file lock-file pref-dir)
                (let ([filename (or filename (find-system-path 'pref-file))])
                  (let-values ([(base name dir?) (split-path filename)])
                    (let ([dir (if (symbol? base)
                                   (current-directory)
                                   base)])
                      (unless (directory-exists? dir)
                        (make-directory* dir))
                      (values
                       filename
                       (make-lock-file-name dir name)
                       dir))))])
    (call-with-preference-file-lock
     'put-preferences
     'exclusive
     (lambda () lock-file)
     (lambda ()
       (let ([f (get-prefs #t filename #f #f)])
         (set! f (let loop ([f f][a null])
                   (cond
                    [(null? f) (reverse
                                (append (map list names vals)
                                        a))]
                    [else (if (memq (caar f) names)
                              (loop (cdr f) a)
                              (loop (cdr f) (cons (car f) a)))])))
         ;; To write the file, copy the old one to a temporary name
         ;; (preserves permissions, etc), write to the temp file,
         ;; then move (atomicly) the temp file to the normal name.
         (let ([tmp-file (make-temporary-file
                          "TMPPREF~a"
                          (and (file-exists? pref-file) pref-file)
                          pref-dir)])
           ;; If something goes wrong, try to delete the temp file.
           (with-handlers ([exn:fail? (lambda (exn)
                                        (with-handlers ([exn:fail:filesystem? void])
                                          (delete-file tmp-file))
                                        (raise exn))])
             ;; Write to temp file...
             (with-output-to-file tmp-file
               #:exists 'truncate/replace
               (lambda ()
                 (with-pref-params
                  (lambda ()
                    ;; If a pref value turns out to be unreadable, raise
                    ;;  an exception instead of creating a bad pref file.
                    (parameterize ([print-unreadable #f])
                      ;; Poor man's pretty-print: one line per entry.
                      (printf "(\n")
                      (for-each (lambda (a)
                                  (if (and (list? (cadr a))
                                           (< 4 (length (cadr a))))
                                      (begin
                                        (printf " (~s\n  (\n" (car a))
                                        (for-each (lambda (i) (printf "   ~s\n" i)) (cadr a))
                                        (printf "  ))\n"))
                                      (printf " ~s\n" a)))
                                f)
                      (printf ")\n"))))))
             ;; Install the new table in the cache. It's possible that this
             ;; cache entry will be replaced by a reading thread before we
             ;; move the file, but that's ok. It just means that a future
             ;; reading thread will have to read again.
             (pref-cache-install! (path->complete-path
                                   (or filename
                                       (find-system-path 'pref-file)))
                                  tmp-file
                                  f)
             (rename-file-or-directory tmp-file pref-file #t)))))
     lock-there)))

;; fold-files : (pathname sym alpha -> alpha) alpha pathname/#f -> alpha
(define (fold-files f init [path #f] [follow-links? #t])
  (define-syntax-rule (keep-fst e)
    (call-with-values (lambda () e) (case-lambda [(v) v] [(v _) v])))
  (define (do-path path acc)
    (cond [(and (not follow-links?) (link-exists? path))
           (keep-fst (f path 'link acc))]
          [(directory-exists? path)
           (call-with-values (lambda () (f path 'dir acc))
               (lambda (acc [descend? #t])
                 (if descend?
                   (do-paths (map (lambda (p) (build-path path p))
                                  (sorted-dirlist path))
                             acc)
                   acc)))]
          [(file-exists? path) (keep-fst (f path 'file acc))]
          [(link-exists? path) (keep-fst (f path 'link acc))] ; dangling links
          [else (error 'fold-files "path disappeared: ~e" path)]))
  (define (do-paths paths acc)
    (cond [(null? paths) acc]
          [else (do-paths (cdr paths) (do-path (car paths) acc))]))
  (define (to-path s) (if (path? s) s (string->path s)))
  (if path (do-path (to-path path) init) (do-paths (sorted-dirlist) init)))

(define (find-files f [path #f] #:follow-links? [follow-links? #t])
  (reverse
   (fold-files (lambda (path kind acc) (if (f path) (cons path acc) acc))
               null path
               follow-links?)))

(define (pathlist-closure paths #:follow-links? [follow-links? #f])
  (let loop ([paths
              (map (lambda (p)
                     (simplify-path
                      (if (and follow-links?
                               (link-exists? p))
                        (let ([p2 (resolve-path p)])
                          (if (relative-path? p2)
                            (let-values ([(base name dir?) (split-path p)])
                              (build-path base p2))
                            p2))
                        p)
                      #f))
                   paths)]
             [r '()])
    (if (null? paths)
      (reverse r)
      (let loop2 ([path (car paths)]
                  [new (cond [(and (not follow-links?)
                                   (link-exists? (car paths)))
                              (list (car paths))]
                             [(file-exists? (car paths))
                              (list (car paths))]
                             [(directory-exists? (car paths))
                              (find-files void (car paths) #:follow-links? follow-links?)]
                             [else (error 'pathlist-closure
                                          "file/directory not found: ~a"
                                          (car paths))])])
        (let-values ([(base name dir?) (split-path path)])
          (if (path? base)
            (loop2 base (if (or (member base r) (member base paths))
                          new (cons base new)))
            (loop (cdr paths) (append (reverse new) r))))))))

(define (check-path who f)
  (unless (path-string? f)
    (raise-argument-error who "path-string?" f)))

(define (check-file-mode who file-mode)
  (unless (memq file-mode '(binary text))
    (raise-argument-error who "(or/c 'binary 'text)" file-mode)))

(define (file->x who f file-mode read-x x-append)
  (check-path who f)
  (check-file-mode who file-mode)
  (let ([sz (file-size f)])
    (call-with-input-file* f #:mode file-mode
      (lambda (in)
        ;; There's a good chance that `file-size' gets all the data:
        (let ([s (read-x sz in)])
          ;; ... but double-check:
          (let ([more (let loop ()
                        (let ([l (read-x 4096 in)])
                          (if (eof-object? l) null (cons l (loop)))))])
            (if (null? more) s (apply x-append (cons s more)))))))))

(define (file->string f #:mode [mode 'binary])
  (file->x 'file->string f mode read-string string-append))

(define (file->bytes f #:mode [mode 'binary])
  (file->x 'file->bytes f mode read-bytes bytes-append))

(define (file->value f #:mode [file-mode 'binary])
  (check-path 'file->value f)
  (check-file-mode 'file->value file-mode)
  (call-with-input-file* f #:mode file-mode read))

(define (file->list f [r read] #:mode [file-mode 'binary])
  (check-path 'file->list f)
  (check-file-mode 'file->list file-mode)
  (unless (and (procedure? r) (procedure-arity-includes? r 1))
    (raise-argument-error 'file->list "(procedure-arity-includes/c 1)" r))
  (call-with-input-file* f #:mode file-mode
    (lambda (p) (for/list ([v (in-port r p)]) v))))

(define (file->x-lines who f line-mode file-mode read-line)
  (check-path who f)
  (check-mode who line-mode)
  (check-file-mode who file-mode)
  (call-with-input-file* f #:mode file-mode
    (lambda (p) (port->x-lines who p line-mode read-line))))

(define (file->lines f #:line-mode [line-mode 'any] #:mode [file-mode 'binary])
  (file->x-lines 'file->lines f line-mode file-mode read-line))

(define (file->bytes-lines f #:line-mode [line-mode 'any] #:mode [file-mode 'binary])
  (file->x-lines 'file->bytes-lines f line-mode file-mode read-bytes-line))

(define (->file who f mode exists write)
  (unless (path-string? f)
    (raise-argument-error who "path-string?" f))
  (unless (memq mode '(binary text))
    (raise-argument-error who "(or/c 'binary 'text)" mode))
  (unless (memq exists '(error append update replace truncate truncate/replace))
    (raise-argument-error who "(or/c 'error 'append 'update 'replace 'truncate 'truncate/replace)" exists))
  (call-with-output-file* f #:mode mode #:exists exists write))

(define (display-to-file s f #:mode [mode 'binary] #:exists [exists 'error])
  (->file 'display-to-file f mode exists (lambda (p) (display s p))))

(define (write-to-file s f #:mode [mode 'binary] #:exists [exists 'error])
  (->file 'write-to-file f mode exists (lambda (p) (write s p))))

(define (display-lines-to-file l f
                               #:mode [mode 'binary]
                               #:exists [exists 'error]
                               #:separator [newline #"\n"])
  (unless (list? l)
    (raise-argument-error 'display-lines-to-file "list?" l))
  (->file 'display-lines-to-file f mode exists
          (lambda (p) (do-lines->port l p newline))))

(define user-read-bit     #o400)
(define user-write-bit    #o200)
(define user-execute-bit  #o100)
(define group-read-bit    #o040)
(define group-write-bit   #o020)
(define group-execute-bit #o010)
(define other-read-bit    #o004)
(define other-write-bit   #o002)
(define other-execute-bit #o001)
