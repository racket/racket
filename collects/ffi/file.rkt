#lang racket/base
(require ffi/unsafe)
(provide security-guard-check-file
         _file/guard
         _file/r
         _file/rw)

(define SCHEME_GUARD_FILE_READ     #x1)
(define SCHEME_GUARD_FILE_WRITE    #x2)
(define SCHEME_GUARD_FILE_EXECUTE  #x4)
(define SCHEME_GUARD_FILE_DELETE   #x8)
(define SCHEME_GUARD_FILE_EXISTS  #x10)

(define scheme_security_check_file
  (get-ffi-obj "scheme_security_check_file" (ffi-lib #f)
               (_fun _symbol _path _int -> _void)))

(define (convert-modes who guards)
  (unless (list? guards)
    (raise-argument-error who "(listof symbol?)" guards))
  (let ([read?    0]
        [write?   0]
        [execute? 0]
        [delete?  0]
        [exists?  0])
    (for-each (lambda (guard)
                (case guard
                  ((read) (set! read? SCHEME_GUARD_FILE_READ))
                  ((write) (set! write? SCHEME_GUARD_FILE_WRITE))
                  ((execute) (set! execute? SCHEME_GUARD_FILE_EXECUTE))
                  ((delete) (set! delete? SCHEME_GUARD_FILE_DELETE))
                  ((exists) (set! exists? SCHEME_GUARD_FILE_EXISTS))
                  (else (raise-arguments-error who "bad permission symbol" "symbol" guard))))
              guards)
    (when (and (positive? exists?)
               (positive? (+ read? write? execute? delete?)))
      (raise-arguments-error who "permission 'exists must occur alone" 
                             "permissions" guards))
    (+ read? write? execute? delete? exists?)))

(define (security-guard-check-file who path modes)
  (unless (symbol? who)
    (raise-argument-error 'security-guard-check-file "symbol?" 0 who path modes))
  (unless (or (path? path) (path-string? path))
    (raise-argument-error 'security-guard-check-file "path-string?" 1 who path modes))
  (let ([cp (cleanse-path (path->complete-path path))]
        [mode (convert-modes 'security-guard-check-file modes)])
    (scheme_security_check_file who cp mode)))

(define (_file/guard modes [who '_file/guard])
  (let ([mode (convert-modes '_file/guard modes)])
    (unless (symbol? who)
      (raise-argument-error '_file/guard "symbol?" who))
    (make-ctype
     _path
     (lambda (p)
       (let ([cp (cleanse-path (path->complete-path p))])
         (scheme_security_check_file who cp mode)
         cp))
     #f)))

(define _file/r (_file/guard '(read) '_file/r))
(define _file/rw (_file/guard '(read write) '_file/rw))

#|
;; -- Tests --

(require rackunit
         racket/runtime-path)

(define-runtime-module-path pub-mod0 racket/list)
(define-runtime-module-path priv-mod0 racket/private/stx)

(define pub-mod (resolved-module-path-name pub-mod0))
(define priv-mod (resolved-module-path-name priv-mod0))

(define (mk-fun modes)
  ;; receives path pointer, casts as int, who cares
  (get-ffi-obj "scheme_make_integer_value" (ffi-lib #f)
               (_fun (path) ::
                     (path : (_file/guard modes))
                     -> _scheme)))

(define (fun path modes)
  ((mk-fun modes) path))

(define sg0 (current-security-guard))

(define sg-ro
  (make-security-guard
   sg0
   (lambda (who path modes)
     (when (or (memq 'write modes) (memq 'delete modes))
       (error who "write/delete not allowed")))
   void void))

(define sg-priv
  (make-security-guard
   sg0
   (lambda (who path modes)
     (when (and path (regexp-match #rx"private" (path->string path)))
       (error who "no access to private paths: ~e" path)))
   void void))

;; Test works on both strings and paths, rel and abs.

(define-syntax-rule (check-ok expr) (check-not-exn (lambda () expr)))
(define-syntax-rule (check-err expr) (check-exn exn:fail? (lambda () expr)))

(define-syntax-rule (run1 expr ok?)
  (void
   (if ok?
       (check-not-exn (lambda () expr))
       (check-exn exn:fail? (lambda () expr)))))

(define (run path modes ok?)
  (run1 (security-guard-check-file 'me path modes) ok?)
  (run1 (fun path modes) ok?))

(test-case "default security guard"
  (parameterize ((current-security-guard sg0))
    (run "foo.txt" '(read) #t)
    (run "bar.txt" '(write delete) #t)
    (run pub-mod '(read) #t)
    (run pub-mod '(write) #t)
    (run priv-mod '(read) #t)
    (run priv-mod '(read write delete) #t)))

(test-case "read-only security-guard"
  (parameterize ((current-security-guard sg-ro))
    (run "foo.txt" '(read) #t)
    (run "bar.txt" '(write delete) #f)
    (run pub-mod '(read) #t)
    (run pub-mod '(write) #f)
    (run priv-mod '(read) #t)
    (run priv-mod '(read write delete) #f)))

(test-case "private security-guard"
  (parameterize ((current-security-guard sg-priv))
    (run pub-mod '(read) #t)
    (run pub-mod '(write) #t)
    (run priv-mod '(read) #f)
    (run priv-mod '(read write delete) #f)))

(provide (all-defined-out))
|#
