#lang racket/base
(require "path.rkt"
         setup/dirs
         setup/cross-system
         (for-syntax racket/base
                     setup/path-to-relative))

(provide delete-directory/files
         copy-directory/files
         make-directory*
         make-parent-directory*

         make-temporary-file
         make-temporary-directory
         make-temporary-file*
         make-temporary-directory*

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

         file-type-bits
         socket-type-bits
         symbolic-link-type-bits
         regular-file-type-bits
         block-device-type-bits
         directory-type-bits
         character-device-type-bits
         fifo-type-bits
         set-user-id-bit
         set-group-id-bit
         sticky-bit
         user-permission-bits
         user-read-bit
         user-write-bit
         user-execute-bit
         group-permission-bits
         group-read-bit
         group-write-bit
         group-execute-bit
         other-permission-bits
         other-read-bit
         other-write-bit
         other-execute-bit)

(require "private/portlines.rkt")

(define (delete-directory/files path
                                #:must-exist? [must-exist? #t])
  (unless (path-string? path)
    (raise-argument-error 'delete-directory/files "path-string?" path))
  (let loop ([path path])
    (case (file-or-directory-type path)
      [(file link)
       (delete-file* path)]
      [(directory)
       (for-each (lambda (e) (loop (build-path path e)))
                 (directory-list path))
       (delete-directory path)]
      [(directory-link)
       (delete-directory path)]
      [else
       (when must-exist?
         (raise-not-a-file-or-directory 'delete-directory/files path))])))

(define (delete-file* path)
  (cond
    [(eq? 'windows (system-type))
     ;; Deleting a file doesn't remove the file name from the
     ;; parent directory until all references are closed, and
     ;; other processes (like the search indexer) might open
     ;; files. So, try to move a file to the temp directory,
     ;; then delete from there. That way, the enclosing directory
     ;; can still be deleted. The move might fail if the
     ;; temp directory is on a different volume, though.
     (define tmp (make-temporary-file))
     (unless (with-handlers ([exn:fail:filesystem? (lambda (x) #f)])
               (rename-file-or-directory path tmp #t)
               #t)
       (delete-file path))
     (delete-file tmp)]
    [else (delete-file path)]))

(define (raise-not-a-file-or-directory who path)
  (raise
   (make-exn:fail:filesystem
    (format "~a: encountered path that is neither file nor directory\n  path: ~a"
            who
            path)
    (current-continuation-marks))))

(define (copy-directory/files src dest
                              #:keep-modify-seconds? [keep-modify-seconds? #f]
                              #:preserve-links? [preserve-links? #f])
  (let loop ([src src] [dest dest])
    (cond [(and preserve-links?
                (link-exists? src))
           (make-file-or-directory-link
            (resolve-path src)
            dest)]
          [(file-exists? src)
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
                     (directory-list src))]
          [else (raise-not-a-file-or-directory 'copy-directory/files src)])))

(define (make-directory* dir)
  (unless (path-string? dir)
    (raise-argument-error 'make-directory* "path-string?" dir))
  (let-values ([(base name dir?) (split-path dir)])
    (when (and (path? base)
               (not (directory-exists? base)))
      (make-directory* base))
    (unless (directory-exists? dir)
      (with-handlers ([exn:fail:filesystem:exists? void])
        (make-directory dir)))))

(define (make-parent-directory* p)
  (unless (path-string? p)
    (raise-argument-error 'make-parent-directory* "path-string?" p))
  (define-values (base name dir?) (split-path p))
  (cond
   [(path? base) (make-directory* base)]
   [else
    ;; Do nothing with an immediately relative path or a root directory
    (void)]))

;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
;;
;; MAKING TEMPORARY FILES & DIRECTORIES
;; ------------------------------------
;;
;; There are four related functions defined in this section:
;;
;;   1. make-temporary-file        \
;;                                  > based on `format`
;;   2. make-temporary-directory   /
;;
;;   3. make-temporary-file*       \
;;                                  > based on `bytes-append`
;;   4. make-temporary-directory*  /
;;
;; All of these are actually macros, to support template generation from context.
;; The macro `define-temporary-file/directory-transformer` and compile-time function
;; `temporary-file/directory-transformer` assist in defining those macros,
;; mostly because we don't have `syntax-parse` here.
;;
;; The core logic is in `internal-make-temporary-file/directory`, which essentially
;; implements the old `make-temporary-file` protocol (before it supported keyword
;; arguments and had a separate `-directory` variant), except that it accepts a
;; function to build candidate file names.
;;
;; Then there is a lot of argument checking.
;; The entry point for the variants based on `format` (no `*`) is
;; `do-make-temporary-file/directory:format`; for the `*` variants,
;; `do-make-temporary-file/directory:bytes-append`.
;; Each entry point tail-calls `do-make-temporary-file/directory:check-make-name`,
;; which tries building a path from the template or prefix+suffix and reports
;; various possible errors with the result.
;; If the tests pass, it tail-calls `internal-make-temporary-file/directory`.

(define (internal-make-temporary-file/directory who copy-from base-dir make-name)
  (define tmpdir (find-system-path 'temp-dir))
  (let loop ([s (current-seconds)]
             [ms (inexact->exact (truncate (current-inexact-milliseconds)))]
             [tries 0])
    (define pth
      (path->complete-path
       (let ([n (make-name
                 ;; docs promise argument will be a string containing only digits
                 (format "~a~a" s ms))])
         (cond [base-dir (build-path base-dir n)]
               [(relative-path? n) (build-path tmpdir n)]
               [else n]))))
    (with-handlers ([(lambda (exn)
                       ;; FIXME: There should probably be a maximum number of
                       ;; tries for other cases, too.
                       ;; See: https://github.com/racket/racket/pull/3870#discussion_r741177829
                       (or (exn:fail:filesystem:exists? exn)
                           (and (exn:fail:filesystem:errno? exn)
                                (let ([errno (exn:fail:filesystem:errno-errno exn)])
                                  (and (eq? 'windows (cdr errno))
                                       (eqv? (car errno) 5) ; ERROR_ACCESS_DENIED
                                       ;; On Windows, if the target path refers to a file
                                       ;; that has been deleted but is still open
                                       ;; somewhere, then an access-denied error is reported
                                       ;; instead of a file-exists error; there appears
                                       ;; to be no way to detect that it was really a
                                       ;; file-still-exists error. Try again for a while.
                                       ;; There's still a small chance that this will
                                       ;; fail, but it's vanishingly small at 32 tries.
                                       ;; If ERROR_ACCESS_DENIED really is the right
                                       ;; error (e.g., because the target directory is not
                                       ;; writable), we'll take longer to get there.
                                       (tries . < . 32))))))
                     (lambda (x)
                       ;; try again with a new name
                       ;; TODO: should this use `crypto-random-bytes`, like FreeBSD, per SEI CERT?
                       ;; See: https://github.com/racket/racket/pull/3870#issuecomment-957779099
                       (loop (- s (random 10))
                             (+ ms (random 10))
                             (add1 tries)))])
      (if copy-from
          (if (eq? copy-from 'directory)
              (make-directory pth)
              (copy-file copy-from pth))
          (close-output-port (open-output-file pth)))
      pth)))

(define (check-base-dir who base-dir)
  (unless (or (not base-dir) (path-string? base-dir))
    (raise-argument-error who "(or/c path-string? #f)" base-dir)))

(define (check-bytes who x)
  (unless (bytes? x)
    (raise-argument-error who "bytes?" x)))

(define (do-make-temporary-file/directory:check-make-name
         who copy-from base-dir make-name
         #:wrapped-make-name wraped-make-name
         #:complete-with-base-error complete-with-base-error
         #:syntactic-directory-error syntactic-directory-error)
  (define result
    ;; docs promise argument will be a string containing only digits
    (wraped-make-name "0"))
  (when (and base-dir (complete-path? result))
    ;; On Windows, base-dir could be a drive specification,
    ;; in which case it is ok for result to be an absolute path without a drive.
    (complete-with-base-error result))
  (unless (eq? 'directory copy-from)
    (when (let-values ([{_base _name must-be-dir?}
                        (split-path result)])
            must-be-dir?)
      (syntactic-directory-error result)))
  (internal-make-temporary-file/directory who copy-from base-dir make-name))

(define (do-make-temporary-file/directory:format who template copy-from base-dir)
  (unless (or (not copy-from)
              (path-string? copy-from)
              (eq? copy-from 'directory))
    (raise-argument-error who
                          "(or/c path-string? 'directory #f)"
                          copy-from))
  (check-base-dir who base-dir)
  (unless (string? template)
    (raise-argument-error who "string?" template))
  (define (make-name digits-str)
    (format template digits-str))
  (define (bad-result-msg details)
    ;; i.e. the result is valid as path, but not for our purposes
    (string-append "given template produced an invalid result;\n " details))
  (do-make-temporary-file/directory:check-make-name
   who copy-from base-dir make-name
   #:wrapped-make-name
   (λ (digits-str)
     (define result
       (with-handlers ([exn:fail:contract?
                        (lambda (x)
                          (raise-arguments-error
                           who
                           "malformed template"
                           "expected" (unquoted-printing-string
                                       "a format string accepting 1 string argument")
                           "given" template))])
         (make-name digits-str)))
     (unless (path-string? result)
       (raise-arguments-error
        who
        "given template produced an invalid path"
        "promised" (unquoted-printing-string "path-string?")
        "produced" result
        "template" template))
     result)
   #:complete-with-base-error
   (λ (result)
     ;; On Windows, base-dir could be a drive specification,
     ;; in which case it is ok for result to be an absolute path without a drive.
     (raise-arguments-error
      who
      (bad-result-msg "complete path can not be combined with base-dir")
      "template" template
      "produced" result
      "base-dir" base-dir))
   #:syntactic-directory-error
   (λ (result)
     (raise-arguments-error
      who
      (bad-result-msg
       "syntactic directory path not allowed unless copy-from is 'directory")
      "template" template
      "produced" result
      "copy-from" copy-from))))

(define (do-make-temporary-file/directory:bytes-append
         ctxt prefix suffix copy-from base-dir
         #:directory? directory?)
  (define who
    (if directory?
        'make-temporary-directory*
        'make-temporary-file*))
  (check-bytes who prefix)
  (check-bytes who suffix)
  (check-base-dir who base-dir)
  (unless (or directory?
              (not copy-from)
              (path-string? copy-from))
    (raise-argument-error who
                          "(or/c path-string? #f)"
                          copy-from))
  (define (make-name/bytes digits-str)
    (bytes-append prefix
                  ctxt
                  (path-element->bytes (string->path-element digits-str))
                  suffix))
  (define (bad-result-msg details)
    ;; i.e. the result is valid as path, but not for our purposes
    (string-append "given prefix and suffix produced an invalid result;\n " details))
  (do-make-temporary-file/directory:check-make-name
   who copy-from base-dir
   (λ (digits-str)
     (bytes->path (make-name/bytes digits-str)))
   #:wrapped-make-name
   (λ (digits-str)
     (define bs (make-name/bytes digits-str))
     (with-handlers ([exn:fail?
                      (λ (e)
                        (raise-arguments-error
                         who
                         "given prefix and suffix produced an invalid path"
                         "prefix" prefix
                         "suffix" suffix
                         "produced" bs))])
       (bytes->path bs)))
   #:syntactic-directory-error
   (λ (result)
     (raise-arguments-error
      who
      (bad-result-msg "syntactic directory path not allowed")
      "prefix" prefix
      "suffix" suffix
      "produced" result))
   #:complete-with-base-error
   (λ (result)
     ;; On Windows, base-dir could be a drive specification,
     ;; in which case it is ok for result to be an absolute path without a drive.
     (raise-arguments-error
      who
      (bad-result-msg "complete path can not be combined with base-dir")
      "prefix" prefix
      "suffix" suffix
      "produced" result
      "base-dir" base-dir))))

(define-for-syntax (syntax->tmp-context-string stx)
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
  (define sanitize-rx
    ;; Keep this in sync with IS_SPEC_CHAR() from racket/src/bc/src/file.c
    ;; and protect-path-element from racket/src/io/path/protect.rkt.
    ;; In addition to the characters handled by both of those,
    ;; we also need to treat ~ as special, since we may be producing
    ;; format string.
    #rx"[<>:\"/\\|?*~]")
  (define sanitized-str (regexp-replace* sanitize-rx combined-str "-"))
  (define max-len 50) ;; must be even
  (define not-too-long-str
    (cond [(< max-len (string-length sanitized-str))
           (string-append (substring sanitized-str 0 (- (/ max-len 2) 2))
                          "----"
                          (substring sanitized-str
                                     (- (string-length sanitized-str)
                                        (- (/ max-len 2) 2))))]
          [else sanitized-str]))
  not-too-long-str)

(define-for-syntax (infer-temporary-file-template stx)
  (string-append (syntax->tmp-context-string stx) "_~a"))

(define-for-syntax (infer-temporary-file-context-bytes stx)
  ;; We rely on the sanitization above having been used
  ;; successfully by Racket from time immemorial.
  (string->bytes/utf-8 (syntax->tmp-context-string stx)))

;; temporary-file/directory-transformer
;;   : (-> identifier? procedure? (-> syntax? syntax?))
;; Produces a macro transformer procedure.
;; The first argument must be an identifier bound at run-time
;; to the function implementation.
;; The second argument must be a (compile-time) procedure that:
;;   - must have no required keyword arguments;
;;   - must NOT permit arbitrary keyword arguments
;;     (i.e. the second result of procedure-keywords must be a list);
;;   - must accept two by-position arguments, namely, the full syntax object
;;     passed to the macro invocation and the run-time identifier from the
;;     first argument; and
;;   - may accept additional by-position arguments.
;; The accepted keywords of the compile-time procedure argument determine
;; the keywords supported by the resulting macro. The by-position arity of
;; the compile-time procedure, after adjusting for the two required by-position
;; arguments, determines the number of by-position arguments supported by the
;; resulting macro. When the resulting macro is used with a supported combination
;; of keyword and by-position arguments, the compile-time procedure is invoked in
;; tail position with the two fixed by-position arguments, plus syntax objects for
;; the supplied keyword and by-position argument expressions. Otherwise, when the
;; resulting macro is used with an unsupported combination of arguments, it expands
;; to a use of the run-time identifier.
(define-for-syntax ((temporary-file/directory-transformer proc-id infer-proc) stx)
  (define-values (_required-kws allowed-kws)
    (procedure-keywords infer-proc))
  (syntax-case stx ()
    [x
     (identifier? #'x)
     proc-id]
    [(_ . more-stx)
     (or (let loop ([args '()]
                    [kws #hasheq()]
                    [stx* #'more-stx])
           (syntax-case stx* ()
             [()
              (and (procedure-arity-includes? infer-proc (+ 2 (length args)))
                   (keyword-apply infer-proc
                                  (hash-keys kws 'ordered)
                                  (hash-values kws 'ordered)
                                  stx
                                  proc-id
                                  (reverse args)))]
             [(kw-stx val . more-stx)
              (memq (syntax-e #'kw-stx) allowed-kws) ;; implies `keyword?`
              (let ([kw (syntax-e #'kw-stx)])
                (and (not (hash-has-key? kws kw))
                     (not (keyword? (syntax-e #'val)))
                     (loop args (hash-set kws kw #'val) #'more-stx)))]
             [(arg . more-stx)
              (not (keyword? (syntax-e #'arg)))
              ;; don't check `procedure-arity-includes?` here,
              ;; because there may be a minimum number of required
              ;; by-position arguments
              (loop (cons #'arg args) kws #'more-stx)]
             [_
              #f]))
         #`(#,proc-id . more-stx))]))

;; define-temporary-file/directory-transformer
;; Definition form of `temporary-file/directory-transformer`,
;; with support for giving the right inferred name to an
;; otherwise-anonymous runtime procedure.
;; See the comments on `temporary-file/directory-transformer` for
;; further details about the compile-time procedure.
(define-syntax (define-temporary-file/directory-transformer stx)
  (syntax-case stx ()
    [(_ name runtime-proc-expr infer-proc-expr)
     (with-syntax ([tmp (datum->syntax #'name (string->symbol (format "~a/proc" (syntax-e #'name))))])
       #`(begin
           (define tmp
             (let ([name runtime-proc-expr])
               name))
           (define-syntax name
             (temporary-file/directory-transformer #'tmp infer-proc-expr))))]))

(define-temporary-file/directory-transformer make-temporary-file
  (λ ([template "rkttmp~a"]
      #:copy-from [copy-from #f]
      #:base-dir [base-dir #f]
      [compat-copy-from copy-from]
      [compat-base-dir base-dir])
    (do-make-temporary-file/directory:format 'make-temporary-file
                                             template compat-copy-from compat-base-dir))
  (λ (#:copy-from [copy-from #''#f]
      #:base-dir [base-dir #''#f]
      stx proc-id)
    #`(#%app #,proc-id
             '#,(infer-temporary-file-template stx)
             #,copy-from
             #,base-dir)))

(define-temporary-file/directory-transformer make-temporary-directory
  (λ ([template "rkttmp~a"]
      #:base-dir [base-dir #f])
    (do-make-temporary-file/directory:format 'make-temporary-directory
                                             template 'directory base-dir))
   (λ (#:base-dir [base-dir #''#f]
       stx proc-id)
     #`(#%app #,proc-id
              '#,(infer-temporary-file-template stx)
              #:base-dir #,base-dir)))

(define-temporary-file/directory-transformer make-temporary-file*
  (λ (#:copy-from [copy-from #f]
      #:base-dir [base-dir #f]
      prefix suffix)
    (do-make-temporary-file/directory:bytes-append
     #"" prefix suffix copy-from base-dir
     #:directory? #f))
  (λ (#:copy-from [copy-from #''#f]
      #:base-dir [base-dir #''#f]
      stx proc-id prefix suffix)
    #`(#%app do-make-temporary-file/directory:bytes-append
             #,(infer-temporary-file-context-bytes stx)
             #,prefix #,suffix #,copy-from #,base-dir
             #:directory? #f)))

(define-temporary-file/directory-transformer make-temporary-directory*
  (λ (#:base-dir [base-dir #f]
      prefix suffix)
    (do-make-temporary-file/directory:bytes-append
     #"" prefix suffix 'directory base-dir
     #:directory? #t))
  (λ (#:base-dir [base-dir #''#f]
      stx proc-id prefix suffix)
    #`(#%app do-make-temporary-file/directory:bytes-append
             #,(infer-temporary-file-context-bytes stx)
             #,prefix #,suffix 'directory #,base-dir
             #:directory? #t)))


;;  Open a temporary path for writing, automatically renames after,
;;  and arranges to delete path if there's an exception. Uses the an
;;  extra rename dance as needed under Windows to ensure that any
;;  existing readers of the file do not prevent updating the
;;  file. Breaks are managed so that the port is reliably closed and
;;  the file is reliably deleted if there's a break.
(define (call-with-atomic-output-file path 
                                      proc
                                      #:security-guard [guard #f]
                                      #:rename-fail-handler [rename-fail-handler #f])
  (unless (path-string? path)
    (raise-argument-error 'call-with-atomic-output-file "path-string?" path))
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 2))
    (raise-argument-error 'call-with-atomic-output-file "(procedure-arity-includes/c 2)" proc))
  (unless (or (not guard)
              (security-guard? guard))
    (raise-argument-error 'call-with-atomic-output-file "(or/c #f security-guard?)" guard))
  (unless (or (not rename-fail-handler)
              (procedure? rename-fail-handler)
              (procedure-arity-includes? rename-fail-handler 2))
    (raise-argument-error 'call-with-atomic-output-file "(or/c #f (procedure-arity-includes/c 2))" rename-fail-handler))
  (define (try-delete-file path [noisy? #t])
    ;; Attempt to delete, but give up if it doesn't work:
    (with-handlers ([exn:fail:filesystem? void])
      (delete-file path)))
  (let ([bp (current-break-parameterization)]
        [tmp-path (parameterize ([current-security-guard (or guard (current-security-guard))])
                    (make-temporary-file #:base-dir (or (path-only path) (current-directory))))]
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
             (with-handlers ([void (lambda (exn)
                                     (try-delete-file tmp-path)
                                     (raise exn))])
               (if (eq? (system-type) 'windows)
                   (cond
                     [rename-fail-handler
                      (let loop ()
                        (with-handlers* ([exn:fail:filesystem?
                                          (lambda (exn)
                                            (call-with-break-parameterization
                                             bp
                                             (lambda () (rename-fail-handler exn tmp-path)))
                                            (loop))])
                          (rename-file-or-directory tmp-path path #t)
                          void))]
                     [else
                      (let ([tmp-path2 (make-temporary-file #:base-dir (path-only path))])
                        (with-handlers ([exn:fail:filesystem? void])
                          (rename-file-or-directory path tmp-path2 #t))
                        (rename-file-or-directory tmp-path path #t)
                        (try-delete-file tmp-path2))])
                   (rename-file-or-directory tmp-path path #t)))
             (try-delete-file tmp-path)))))))

(define (with-pref-params thunk)
  (parameterize ([read-case-sensitive #f]
                 [read-square-bracket-as-paren #t]
                 [read-curly-brace-as-paren #t]
                 [read-square-bracket-with-tag #f]
                 [read-curly-brace-with-tag #f]
                 [read-accept-box #t]
                 [read-accept-compiled #f]
                 [read-accept-bar-quote #t]
                 [read-accept-graph #t]
                 [read-decimal-as-inexact #t]
                 [read-cdot #f]
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
    (if (eq? 'windows (cross-system-type))
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
                                  (directory-list path))
                             acc)
                   acc)))]
          [(file-exists? path) (keep-fst (f path 'file acc))]
          [(link-exists? path) (keep-fst (f path 'link acc))] ; dangling links
          [else (error 'fold-files "path disappeared: ~e" path)]))
  (define (do-paths paths acc)
    (cond [(null? paths) acc]
          [else (do-paths (cdr paths) (do-path (car paths) acc))]))
  (define (to-path s) (if (path? s) s (string->path s)))
  (if path (do-path (to-path path) init) (do-paths (directory-list) init)))

(define (find-files f [path #f]
                    #:follow-links? [follow-links? #t]
                    #:skip-filtered-directory? [skip-filtered-directory? #f])
  (reverse
   (fold-files (lambda (path kind acc) (if (f path)
                                      (cons path acc) 
                                      (if (and skip-filtered-directory?
                                               (eq? kind 'dir))
                                          (values acc #f)
                                          acc)))
               null path
               follow-links?)))

(define (pathlist-closure paths
                          #:follow-links? [follow-links? #f]
                          #:path-filter [path-filter #f])
  (let loop ([paths
              (map (lambda (p)
                     (simplify-path
                      (let loop ([p p])
                        (if (and follow-links?
                                 (link-exists? p))
                            (let ([p2 (resolve-path p)])
                              (if (relative-path? p2)
                                  (let-values ([(base name dir?) (split-path p)])
                                    (loop ((if dir? path->directory-path values)
                                           (if (path? base)
                                               (build-path base p2)
                                               p2))))
                                  (loop p2)))
                            p))
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
                              (find-files (or path-filter void)
                                          (path->directory-path (car paths))
                                          #:skip-filtered-directory? #t
                                          #:follow-links? follow-links?)]
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

(define (file->x who f file-mode read-x x-append empty-val)
  (check-path who f)
  (check-file-mode who file-mode)
  (let ([sz (with-handlers ([exn:fail:filesystem? (lambda (_) 0)])
                             (file-size f))])
    (call-with-input-file* f #:mode file-mode
      (lambda (in)
        ;; There's a good chance that `file-size' gets all the data:
        (let ([s (read-x sz in)])
          (if (eof-object? s)
              ;; the file was truncated to size 0 _after_ we got the
              ;; file size
              empty-val
              ;; ... check for more data past the initial file-size amt
              (let ([more (let loop ()
                            (let ([l (read-x 4096 in)])
                              (if (eof-object? l) null (cons l (loop)))))])
                (if (null? more) s (apply x-append (cons s more))))))))))

(define (file->string f #:mode [mode 'binary])
  (file->x 'file->string f mode read-string string-append ""))

(define (file->bytes f #:mode [mode 'binary])
  (file->x 'file->bytes f mode read-bytes bytes-append #""))

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

; See https://en.wikibooks.org/wiki/C_Programming/POSIX_Reference/sys/stat.h
(define file-type-bits             #o170000)
(define socket-type-bits           #o140000)
(define symbolic-link-type-bits    #o120000)
(define regular-file-type-bits     #o100000)
(define block-device-type-bits     #o060000)
(define directory-type-bits        #o040000)
(define character-device-type-bits #o020000)
(define fifo-type-bits             #o010000)
(define set-user-id-bit            #o004000)
(define set-group-id-bit           #o002000)
(define sticky-bit                 #o001000)
(define user-permission-bits       #o000700)
(define user-read-bit              #o000400)
(define user-write-bit             #o000200)
(define user-execute-bit           #o000100)
(define group-permission-bits      #o000070)
(define group-read-bit             #o000040)
(define group-write-bit            #o000020)
(define group-execute-bit          #o000010)
(define other-permission-bits      #o000007)
(define other-read-bit             #o000004)
(define other-write-bit            #o000002)
(define other-execute-bit          #o000001)
