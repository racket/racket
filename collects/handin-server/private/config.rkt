#lang racket/base

(require racket/file)

;; This module should be invoked when we're in the server directory
(provide server-dir)
(define server-dir
  (let ([dir (or (getenv "PLT_HANDINSERVER_DIR") (current-directory))])
    (if (directory-exists? dir)
      dir
      (error 'config "handin server directory does not exist: ~e" dir))))

(define config-file (path->complete-path "config.rktd" server-dir))

(define poll-freq 2000.0) ; poll at most once every two seconds

(define last-poll     #f)
(define last-filetime #f)
(define raw-config    #f)
(define config-cache  #f)

(provide get-conf)
(define (get-conf key)
  (unless (and raw-config
               (< (- (current-inexact-milliseconds) last-poll) poll-freq))
    (set! last-poll (current-inexact-milliseconds))
    (let ([filetime (file-or-directory-modify-seconds config-file)])
      (unless (and filetime (equal? filetime last-filetime))
        (set! last-filetime filetime)
        (with-handlers
            ([void (lambda (e)
                     (raise-user-error 'get-conf "could not read conf (~a): ~a"
                                       config-file (exn-message e)))])
          (when raw-config
            ;; can't use log-line from logger, since it makes a cycle,
            ;; but make sure it's written in one shot; in any case, don't
            ;; write anything if this is the first read, since the logger
            ;; is not initialized yet (and if there's an error at this
            ;; stage, the server will exit)
            (eprintf (format "reloading configuration from ~a\n" config-file)))
          (let ([c (with-input-from-file config-file read)])
            (if (and (list? c)
                     (andmap (lambda (x)
                               (and (pair? x) (symbol? (car x))))
                             c))
              (set! raw-config c)
              (raise-user-error
               'get-conf "malformed configuration file content"))))
        (set! config-cache (make-hasheq)))))
  (hash-ref config-cache key
    (lambda ()
      (define-values [default translate] (config-default+translate key))
      (define v
        (case translate
          ;; #f => computed value => return untranslated default w/out lookup
          [(#f) default]
          ;; #t => user key => return raw value or error
          [(#t) (cond [(assq key raw-config) => cadr]
                      [else (raise-user-error
                             'get-conf "no value for key: ~e" key)])]
          [else (translate (cond [(assq key raw-config) => cadr]
                                 [else default]))]))
      (hash-set! config-cache key v)
      v)))

(define (id x)         x)
(define (rx s)         (if (regexp? s) s (regexp s)))
(define (path p)       (path->complete-path p server-dir))
(define (path/false p) (and p (path p)))
(define (path-list l)  (map path l))
(define (maybe-strs l) (and l (pair? l) (map string->bytes/utf-8 l)))

(define (config-default+translate which)
  ;; translate = #f => a computed value (so no lookup or translation)
  ;;           = #t => an unknown key (raw return value)
  (case which
    [(active-dirs)             (values '()                   path-list    )]
    [(inactive-dirs)           (values '()                   path-list    )]
    [(port-number)             (values 7979                  id           )]
    [(use-https)               (values #t                    id           )]
    [(hook-file)               (values #f                    path/false   )]
    [(session-timeout)         (values 300                   id           )]
    [(session-memory-limit)    (values 40000000              id           )]
    [(default-file-name)       (values "handin.rkt"          id           )]
    [(max-upload)              (values 500000                id           )]
    [(max-upload-keep)         (values 9                     id           )]
    [(user-regexp)             (values #rx"^[a-z][a-z0-9]+$" rx           )]
    [(user-desc)               (values "alphanumeric string" id           )]
    [(username-case-sensitive) (values #f                    id           )]
    [(allow-new-users)         (values #f                    id           )]
    [(allow-change-info)       (values #f                    id           )]
    [(allow-web-upload)        (values #f                    maybe-strs   )]
    [(master-password)         (values #f                    id           )]
    [(log-output)              (values #t                    id           )]
    [(log-file)                (values "log"                 path/false   )]
    [(web-log-file)            (values #f                    path/false   )]
    [(extra-fields)
     (values '(("Full Name" #f #f)
               ("ID#" #f #f)
               ("Email" #rx"^[^@<>\"`',]+@[a-zA-Z0-9_.-]+[.][a-zA-Z]+$"
                "a valid email address"))
             id)]
    ;; computed from the above (mark by translate = #f)
    [(all-dirs)
     (values (append (get-conf 'active-dirs) (get-conf 'inactive-dirs)) #f)]
    [(names-dirs) ; see below
     (values (paths->map (get-conf 'all-dirs)) #f)]
    [(user-fields)
     (values (filter (lambda (f) (not (eq? '- (cadr f))))
                     (get-conf 'extra-fields))
             #f)]
    [else (values #f #t)]))

;; This is used below to map names to submission directory paths and back
;; returns a (list-of (either (list name path) (list path name)))
(define (paths->map dirs)
  (define (path->name dir)
    (unless (directory-exists? dir)
      (if (file-exists? dir)
        (error 'get-conf "directory entry points at a file: ~e" dir)
        (make-directory* dir)))
    (let-values ([(_1 name _2) (split-path dir)])
      (path-element->string name)))
  (let ([names (map path->name dirs)])
    (append (map list names dirs) (map list dirs names))))

;; Translates an assignment name to a directory path or back
(provide assignment<->dir)
(define (assignment<->dir a/d)
  (cond [(assoc a/d (get-conf 'names-dirs)) => cadr]
        [else (error 'assignment<->dir "internal error: ~e" a/d)]))
