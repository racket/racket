#lang racket/base
(require syntax/modread
         racket/file
         compiler/compilation-path
         compiler/cm)

(define (go dir collects-dir fn)
  (parameterize ([current-load-relative-directory dir])
    (define path (build-path dir fn))
    (define stx
      (check-module-form
       (call-with-input-file*
        path
        (lambda (i)
          (port-count-lines! i)
          (with-module-reading-parameterization
              (lambda ()
                (read-syntax path i)))))
       'mod
       #f))
    (define o (open-output-bytes))
    (parameterize ([current-namespace (make-base-namespace)]
                   [current-write-relative-directory (cons dir collects-dir)])
      (dynamic-require 'racket/private/base #f)
      (write (compile stx) o))
    (define bstr (get-output-bytes o))
    (install-module-hashes! bstr)
    bstr))

(define (check-file dir collects-dir sub-dir f)
  (printf "~a\n" (build-path sub-dir f))
  (define c1 (go dir collects-dir f))
  (define c2 (go dir collects-dir f))
  (unless (equal? c1 c2)
    (call-with-output-file* "/tmp/c1" #:exists 'truncate (lambda (o) (write-bytes c1 o)))
    (call-with-output-file* "/tmp/c2" #:exists 'truncate (lambda (o) (write-bytes c2 o)))    
    (error "failed"))
  (define zo (get-compilation-bytecode-file (build-path dir f) #:modes '("compiled")))
  (when (file-exists? zo)
    (define c3 (file->bytes zo))
    (unless (equal? c3 c1)
      (call-with-output-file* "/tmp/c1" #:exists 'truncate (lambda (o) (write-bytes c1 o)))
      (call-with-output-file* "/tmp/c2" #:exists 'truncate (lambda (o) (write-bytes c3 o)))
      (error "failed relative to built"))))

(define (check dir [collects-dir dir] [sub-dir 'same] #:limit [limit +inf.0])
  (for/fold ([limit limit]) ([f (in-list (directory-list dir))]
                             #:when (positive? limit))
    (cond
     [(and (regexp-match? #rx"[.]rkt$" f)
           (file-exists? (build-path dir f)))
      (check-file dir collects-dir sub-dir f)
      (sub1 limit)]
     [(directory-exists? (build-path dir f))
      (check (build-path dir f) collects-dir (build-path sub-dir f) #:limit limit)]
     [else limit])))

(define (check-one collects-dir collects file)
  (check-file (build-path collects-dir collects) collects-dir collects file))

(let-values ([(dir name dir?)
              (split-path
               (collection-file-path "sc.rkt" "racket/private"))])
  (define collects-dir (simplify-path (build-path dir 'up 'up)))
  ;; To check just one:
  #; (check-one collects-dir "syntax" "free-vars.rkt")
  (check (simplify-path collects-dir))
  (void))

(module+ test
  (module config info
    (define timeout 300)))
