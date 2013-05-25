#lang scheme/base

;; `omitted-paths' returns a list of omitted file and subdirectory names for a
;; given directory, or 'all if the directory is completely omitted.  Considers
;; the local info.rkt as well as info.rkt in parent directories all the way to a
;; collection root.  (Could be a bit easier using `find-relevant-directories',
;; but it needs to be available for setup-plt, before the "info-domain" caches
;; are created.)

(provide omitted-paths)

(require scheme/path scheme/list scheme/promise "../dirs.rkt" "lib-roots.rkt")

;; An entry for each collections root that holds a hash table.  The hash table
;; maps a reversed list of subpath elements to the exploded omitted-paths
;; specified by the info files accumulated at that subpath for that subpath --
;; filtered to only relevant ones.  Some entries are added automatically:
;; "compiled", directories that begin with a ".", and "doc" unless it's in the
;; main collection tree (it is not used there for documentation, and there is
;; at least one place where it contains code: scribble/doc).
(define roots
  (delay
    (map (lambda (p)
           (list (explode-path (simplify-path (car p)))
		 (make-hash)
                 ;; don't omit "doc" in the main tree
                 (not (equal? (find-collects-dir) (car p)))))
         library-roots)))

;; if `x' has `y' as a prefix, return the tail,
;; eg (relative-from '(1 2 3 4) '(1 2)) => '(3 4)
(define (relative-from x y)
  (cond [(null? y) x]
        [(null? x) #f]
        [(equal? (car x) (car y)) (relative-from (cdr x) (cdr y))]
        [else #f]))

(define-syntax-rule (with-memo t x expr)
  (hash-ref! t x (lambda () expr)))

(define ((implicit-omit? omit-doc?) path)
  (let ([str (path-element->string path)])
    (or (member str '("compiled" "CVS"))
        (and omit-doc? (equal? "doc" str))
        (regexp-match? #rx"^[.]" str))))

(define (compute-omitted dir accumulated implicit-omit? get-info/full)
  (define info (or (get-info/full dir) (lambda _ '())))
  (define explicit
    (let ([omit (info 'compile-omit-paths (lambda () '()))])
      (if (eq? 'all omit)
        'all
        (map (lambda (e) (explode-path (simplify-path e)))
             ;; for backward compatibility
             (append omit (info 'compile-omit-files (lambda () '())))))))
  (cond
    [(or (eq? 'all explicit) (memq 'same explicit)) 'all]
    [(findf (lambda (e)
              (or (null? e) (not (path? (car e))) (absolute-path? (car e))))
            explicit)
     => (lambda (bad)
          (error 'compile-omit-paths
                 "bad entry value in info file: ~e" (apply build-path bad)))]
    [else (append explicit
                  (map list (filter implicit-omit? (directory-list dir)))
                  accumulated)]))

(define (accumulate-omitted get-info/full rsubs root t omit-doc?)
  (define dir (apply build-path root))
  (define implicit? (implicit-omit? omit-doc?))
  (let loop ([rsubs rsubs])
    (if (null? rsubs)
      (compute-omitted dir '() implicit? get-info/full)
      (with-memo t rsubs
        (let ([acc (loop (cdr rsubs))])
          (if (or (eq? 'all acc) (member (list (car rsubs)) acc))
            'all
            (compute-omitted (apply build-path dir (reverse rsubs))
                             (for/list ([up acc]
                                        #:when (equal? (car up) (car rsubs)))
                               ;; must have non-null cdr: see `member' check
                               (cdr up))
                             implicit?
                             get-info/full)))))))

(define (omitted-paths* dir get-info/full root-dir)
  (unless (and (path-string? dir) (complete-path? dir) (directory-exists? dir))
    (raise-type-error 'omitted-paths
                      "complete path to an existing directory" dir))
  (let* ([dir* (explode-path (simple-form-path dir))]
         [r (ormap (lambda (root+table)
                     (let ([r (relative-from dir* (car root+table))])
                       (and r (cons (reverse r) root+table))))
                   (if root-dir
                       (list (list (explode-path (simple-form-path root-dir))
                                   (make-hash)
                                   #t))
                       (force roots)))]
         [r (and r (apply accumulate-omitted get-info/full r))])
                
    (unless r
      (error 'omitted-paths
             "given directory path is not in any collection root: ~e" dir))
    (if (eq? 'all r)
      r
      (filter-map (lambda (x) (and (null? (cdr x)) (car x))) r))))

(define omitted-paths-memo (make-hash))

(define (omitted-paths dir get-info/full [root-dir #f])
  (with-memo omitted-paths-memo dir (omitted-paths* dir get-info/full root-dir)))
