#lang racket/base
(require '#%foreign
         (only-in '#%unsafe
                  unsafe-make-security-guard-at-root
                  unsafe-add-post-custodian-shutdown)
         setup/dirs)

(provide (protect-out get-ffi-lib*))

(define lib-suffix (bytes->string/latin-1 (subbytes (system-type 'so-suffix) 1)))
(define lib-suffix-re (regexp (string-append "\\." lib-suffix "$")))
(define suffix-before-version? (and (not (equal? lib-suffix "dylib"))
                                    (not (equal? lib-suffix "dll"))))
(define version-sep (if (equal? lib-suffix "dll") "-" "."))

(define-logger ffi-lib)

(define (get-ffi-lib* name [version/s ""]
                      #:who [who 'ffi-lib]
                      #:fail [fail #f]
                      #:get-lib-dirs [get-lib-dirs get-lib-search-dirs]
                      #:global? [global? (eq? (system-type 'so-mode) 'global)]
                      #:custodian [custodian #f])
  (cond
   [(not name) (ffi-lib name #f #f who)] ; #f => NULL => open this executable
   [(not (or (string? name) (path? name)))
    (raise-argument-error who "(or/c string? path?)" name)]
   [else
    ;; A possible way that this might be misleading: say that there is a
    ;; "foo.so" file in the current directory, which refers to some
    ;; undefined symbol, trying to use this function with "foo.so" will try
    ;; a dlopen with "foo.so" which isn't found, then it tries a dlopen with
    ;; "/<curpath>/foo.so" which fails because of the undefined symbol, and
    ;; since all fails, it will use (ffi-lib "foo.so") to raise the original
    ;; file-not-found error.  This is because the dlopen doesn't provide a
    ;; way to distinguish different errors (only dlerror, but that's
    ;; unreliable).
    (define (fullpath p) (path->complete-path (cleanse-path p)))
    (define tried '()) ;; (listof path-string), mutated
    (define (try-lib name)
      (let ([lib (ffi-lib name #t global? who)])
        (cond [lib (log-ffi-lib-debug "loaded ~e" name)]
              [else (set! tried (cons name tried))])
        lib))
    (define (skip-lib name)
      (begin (set! tried (cons name tried)) #f))
    (define (try-lib-if-exists? name)
      (cond [(file-exists?/insecure name) (try-lib (fullpath name))]
            [else (skip-lib (fullpath name))]))
    (let* ([versions (if (list? version/s) version/s (list version/s))]
	   [versions (map (lambda (v)
			    (if (or (not v) (zero? (string-length v)))
				""
                                (string-append version-sep v)))
			  versions)]
	   [absolute? (absolute-path? name)]
	   [name0 (path->string (cleanse-path name))]     ; orig name
	   [names (map (if (regexp-match lib-suffix-re name0) ; name+suffix
			   (lambda (v) (string-append name0 v))
			   (lambda (v) 
			     (if suffix-before-version?
				 (string-append name0 "." lib-suffix v)
				 (string-append name0 v "." lib-suffix))))
		       versions)])
      (define lib
        (or ;; try to look in our library paths first
         (and (not absolute?)
              (ormap (lambda (dir)
                       ;; try good names first, then original
                       (or (ormap (lambda (name) (try-lib (build-path dir name)))
                                  names)
                           (try-lib (build-path dir name0))))
                     (get-lib-dirs)))
         ;; try a system search
         (ormap try-lib names)              ; try good names first
         (try-lib name0)                    ; try original
         (ormap try-lib-if-exists? names)   ; try relative paths
         (try-lib-if-exists? name0)         ; relative with original
         ;; give up: by default, call ffi-lib so it will raise an error
         (begin
           (log-ffi-lib-debug
            "failed for (ffi-lib ~v ~v), tried: ~a"
            name0 version/s
            (apply
             string-append
             (for/list ([attempt (reverse tried)])
               (format "\n  ~e~a" attempt
                       (cond [(absolute-path? attempt)
                              (cond [(file-exists?/insecure attempt) " (exists)"]
                                    [else " (no such file)"])]
                             [else " (using OS library search path)"])))))
           (and (not fail)
                (if (pair? names)
                    (ffi-lib (car names) #f global? who)
                    (ffi-lib name0 #f global? who))))))
      (cond
        [lib
         (when custodian
           (unsafe-add-post-custodian-shutdown (lambda () (ffi-lib-unload lib))
                                               (if (eq? custodian 'place)
                                                   #f
                                                   custodian)))
         lib]
        [fail
         (fail)]
        [else (error who "internal error; shouldn't get here")]))]))

;; Like `file-exists?`, but avoid security-guard checks on the grounds
;; that it's being called from an already-allowed unsafe operation ---
;; so a sandbox doesn't have to make additional allowances for the
;; check.
(define (file-exists?/insecure path)
  (parameterize ([current-security-guard (unsafe-make-security-guard-at-root)])
    (file-exists? path)))
