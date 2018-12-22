#lang racket/base
(require compiler/depend
         ;; This dependency on `compiler/private/cm-minimal`
         ;; ensure that it's compiled so that the next use
         ;; of "setup-go.rkt" doesn't have to start from source
         compiler/private/cm-minimal)

;; This module is loaded via `setup/main` with a `--boot` argument
;; that selects this module and sets the compile-file root directory
;; to be within the build directory.
;;
;; Overall arguments:
;;
;;   --boot <this-file> <compiled-dir>
;;          <target-file> <dep-file/tag> <mod-file>
;;          <arg> ...
;;
;; where <mod-file> is the file to load (bootstrapping as needed), and
;; the <arg>s are made the command-line argument for <mod-file>. The
;; <target-file> is the output file that <mod-file> generates. The
;; <dep-file/tag> is written as makefile rule for <target-file>, where
;; a "$" is added to the front of <target-file> if it's parenthesized.
;;
;; If <target-file> is `--tag`, then <dep-file/tag> specifies a tag to
;; get stripped form <arg>, there the target file is immediately after
;; the tag. In that case, the dependency file name is formed by using
;; just the file name of the target, replacing the suffix with ".d".
;;
;; The point of going through `setup/main` is that the Racket module
;; gets compiled as needed, so that it doesn't have to be loaded from
;; source every time. At the same time `setup/main` detects when files
;; need to be recompiled, either becuase the underlying Racket's
;; version changed or because a dependency changed.

(provide go)

(define (go orig-compile-file-paths)
  (define SETUP-ARGS 6)
  (define prog-args (list-tail (vector->list (current-command-line-arguments)) SETUP-ARGS))
  (define target-file-spec (vector-ref (current-command-line-arguments) 3))
  (define target-tag (and (equal? target-file-spec "--tag")
                          (vector-ref (current-command-line-arguments) 4)))
  (define target-file (if target-tag
                          (let loop ([l prog-args])
                            (cond
                              [(or (null? l) (null? (cdr l)))
                               (error 'setup-go "could not find target")]
                              [(equal? (car l) target-tag) (cadr l)]
                              [else (loop (cdr l))]))
                          target-file-spec))
  (define make-dep-file (if target-tag
                            (let-values ([(base name dir?) (split-path target-file)])
                              (path-replace-suffix name #".d"))
                            (vector-ref (current-command-line-arguments) 4)))
  (define mod-file (simplify-path (path->complete-path (vector-ref (current-command-line-arguments) 5))))
  (parameterize ([current-command-line-arguments
                  ;; Discard `--boot` through arguments to this
                  ;; module, and also strip `target-tag` (if any).
                  (list->vector (let loop ([l prog-args])
                                  (cond
                                    [(null? l) '()]
                                    [(equal? (car l) target-tag) (cdr l)]
                                    [else (cons (car l) (loop (cdr l)))])))])
    ;; In case multiple xforms run in parallel, use a lock file so
    ;;  that only one is building.
    (define lock-file (build-path (car (current-compiled-file-roots)) "SETUP-LOCK"))
    (define lock-port (open-output-file #:exists 'truncate/replace lock-file))
    (let loop ([n 0])
      (when (= n 3)
        (printf "Waiting on lock: ~a" lock-file))
      (unless (port-try-file-lock? lock-port 'exclusive)
        (sleep 0.1)
        (loop (add1 n))))

    (with-handlers ([exn? (lambda (exn)
                            ;; On any execption, try to delete the target file
                            (with-handlers ([exn:fail:filesystem?
                                             (lambda (exn) (log-error "~s" exn))])
                              (when (file-exists? target-file)
                                (delete-file target-file)))
                            (raise exn))])
      (dynamic-wind
       void
       (lambda ()
         ;; Load the requested module, but don't instantiate:
         (dynamic-require mod-file (void)))
       (lambda ()
         (port-file-unlock lock-port)))

      ;; Record dependencies (before running `mod-file`, in case
      ;; it mangles parameters)
      (define deps (cons mod-file
                         (module-recorded-dependencies mod-file)))
      (define (quote-if-space s)
	;; We're not handling arbitrary paths, but at least support spaces
	(if (regexp-match? #rx" " s) (format "\"~a\"" s) s))
      (call-with-output-file make-dep-file
        #:exists 'truncate
        (lambda (o)
          (fprintf o "~a: " (if (regexp-match? #rx"^[(].*[)]$" target-file)
                                (string-append "$" target-file)
                                (quote-if-space target-file)))
          (for ([dep (in-list deps)])
            (fprintf o " \\\n ~a" (quote-if-space dep)))
          (newline o)
          (for ([dep (in-list deps)])
            (fprintf o "\n~a:\n" (quote-if-space dep)))))

      ;; Now that the lock is released, instantiate:
      (let ([main `(submod ,mod-file main)])
        (if (module-declared? main #t)
            (dynamic-require main #f)
            (dynamic-require mod-file #f))))))
