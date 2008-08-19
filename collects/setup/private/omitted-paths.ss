#lang scheme/base

;; `omitted-paths' returns a list of omitted file and subdirectory names for a
;; given directory, or 'all if the directory is completely omitted.  Considers
;; the local info.ss as well as info.ss in parent directories all the way to a
;; collection root.  (Could be a bit easier using `find-relevant-directories',
;; but it needs to be available for setup-plt, before the "info-domain" caches
;; are created.)

(provide omitted-paths)

(require scheme/path scheme/list "../dirs.ss" "../getinfo.ss"
         (prefix-in planet: planet/config))

;; An entry for each collections root that holds a hash table.  The hash table
;; maps a reversed list of subpath elements to the exploded omitted-paths
;; specified by the info files accumulated at that subpath for that subpath --
;; filtered to only relevant ones.  Some entries are added automatically:
;; "compiled", directories that begin with a ".", and "doc" unless it's in the
;; main collection tree (it is not used there for documentation, and there is
;; at least one place where it contains code: scribble/doc).
(define roots
  (map (lambda (p)
         (list (explode-path p) (make-hash)
               ;; don't omit "doc" in the main tree
               (not (equal? (find-collects-dir) p))))
       (cons (planet:CACHE-DIR) (current-library-collection-paths))))

;; if `x' has `y' as a prefix, return the tail,
;; eg (relative-from '(1 2 3 4) '(1 2)) => '(3 4)
(define (relative-from x y)
  (cond [(null? y) x]
        [(null? x) #f]
        [(equal? (car x) (car y)) (relative-from (cdr x) (cdr y))]
        [else #f]))

(define-syntax-rule (with-memo t x expr)
  (hash-ref t x (lambda () (let ([r expr]) (hash-set! t x r) r))))

(define ((implicit-omit? omit-doc?) path)
  (let ([str (path-element->string path)])
    (or (member str '("compiled" "CVS"))
        (and omit-doc? (equal? "doc" str))
        (regexp-match? #rx"^[.]" str))))

(define (omitted-paths* dir)
  (define (get rsubs root t omit-doc?)
    (let loop ([rsubs rsubs])
      (if (null? rsubs)
        '()
        (with-memo t rsubs
          (let ([up (loop (cdr rsubs))])
            (if (or (eq? 'all up) (member (list (car rsubs)) up))
              'all
              (let* ([dir (apply build-path (append root (reverse rsubs)))]
                     [info (or (get-info/full dir) (lambda _ '()))]
                     [explicit (info 'compile-omit-paths (lambda () '()))]
                     [explicit (if (eq? 'all explicit)
                                 'all
                                 (map (lambda (e)
                                        (explode-path (simplify-path e #f)))
                                      (append explicit
                                              ;; for backward compatibility
                                              (info 'compile-omit-files
                                                    (lambda () '())))))])
                (cond
                  [(or (eq? 'all explicit) (memq 'same explicit)) 'all]
                  [(findf (lambda (e)
                            (or (null? e)
                                (not (path? (car e)))
                                (absolute-path? (car e))))
                          explicit)
                   => (lambda (bad)
                        (error 'compile-omit-paths
                               "bad entry value in info file: ~e"
                               (apply build-path bad)))]
                  [else `(,@explicit
                          ,@(map list (filter (implicit-omit? omit-doc?)
                                              (directory-list dir)))
                          ,@(filter-map (lambda (up)
                                          (and (equal? (car up) (car rsubs))
                                               ;; must have non-null cdr, due
                                               ;; to the above `member' check
                                               (cdr up)))
                                        up))]))))))))
  (unless (and (path-string? dir) (complete-path? dir) (directory-exists? dir))
    (raise-type-error 'omitted-paths
                      "complete path to an existing directory" dir))
  (let* ([dir* (explode-path (simplify-path dir))]
         [r (ormap (lambda (root+table)
                     (let ([r (relative-from dir* (car root+table))])
                       (and r (cons (reverse r) root+table))))
                 roots)]
         [r (and r (apply get r))])
    (unless r
      (error 'omitted-paths
             "given directory path is not in any collection root: ~e" dir))
    (if (eq? 'all r)
      r
      (filter-map (lambda (x) (and (null? (cdr x)) (car x))) r))))

(define omitted-paths-memo (make-hash))

(define (omitted-paths dir)
  (with-memo omitted-paths-memo dir (omitted-paths* dir)))
