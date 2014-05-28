#lang racket/base

(require racket/match 
         racket/contract 
         planet/cachepath 
         syntax/modread
         "dirs.rkt"
         "path-relativize.rkt")

;; in addition to infodomain/compiled/cache.rktd, getinfo will look in this 
;; file to find mappings. PLaneT uses this to put info about installed
;; planet packages.
(define user-infotable (get-planet-cache-path))

;; get-info : (listof path-or-string) -> info/#f
(define (get-info coll-path #:namespace [ns #f] #:bootstrap? [bootstrap? #f])
  (get-info/full (apply collection-path
                        (map (lambda (x) (if (path? x) (path->string x) x))
                             coll-path))
                 #:namespace ns
                 #:bootstrap? bootstrap?))

;; These `require's ensure that the `#lang info' readers
;; are loaded, so that no reader guard will be invoked for the reader
;; intself when checking a language via a reader guard, and 
(require (only-in setup/infotab)
         (only-in info)
         (only-in setup/infotab/lang/reader)
         (only-in (submod info reader)))

;; get-info/full : path -> info/#f
(define (get-info/full dir #:namespace [ns #f] #:bootstrap? [bootstrap? #f])
  (or (get-info/full/ext dir "rkt" ns bootstrap?)
      (get-info/full/ext dir "ss" ns bootstrap?)))

(define (get-info/full/ext dir ext ns bootstrap?)
  (define file (build-path dir (format "info.~a" ext)))
  (define enclosing-ns (variable-reference->namespace
                        (#%variable-reference)))
  (define (err fmt . args)
    (apply error 'get-info (string-append "info file " fmt " in ~a")
           (append args (list file))))
  (define (contents)
    (parameterize ([current-reader-guard
                    (lambda (x)
                      (if (or (eq? x 'setup/infotab/lang/reader)
                              (eq? x 'info/lang/reader)
                              (equal? x '(submod setup/infotab reader))
                              (equal? x '(submod info reader)))
                        x
                        (err "has illegal #lang or #reader")))]
                   [current-namespace
                    ;; Use this module's namespace; see the `only-in'
                    ;; `require's above.
                    enclosing-ns])
      (with-input-from-file file
        (lambda ()
          (begin0 
           (with-module-reading-parameterization read)
           (unless (eof-object? (read))
             (err "has multiple expressions")))))))
  (and (file-exists? file)
       (match (contents)
         [(list 'module 'info
                (or '(lib "infotab.rkt" "setup")
                    '(lib "infotab.ss" "setup")
                    '(lib "setup/infotab.rkt")
                    '(lib "setup/infotab.ss")
                    '(lib "main.rkt" "info")
                    'setup/infotab
                    'info)
                expr ...)
          ;; Although `#lang info` is intended to be loaded as code,
          ;; many modules are so simple that we can synthesize the
          ;; procedure directly:
          (cond
           [(and (not bootstrap?)
                 (= 1 (length expr))
                 (list? (car expr))
                 ((length (car expr)) . >= . 1)
                 (eq? '#%module-begin (caar expr))
                 (for/fold ([ht #hasheq()]) ([e (in-list (cdar expr))])
                   (match e
                     [`(define ,id ,rhs)
                      (and (symbol? id)
                           ht
                           (eq? file (hash-ref ht id file))
                           (or (string? rhs)
                               (number? rhs)
                               (boolean? rhs)
                               (and (pair? rhs)
                                    (eq? 'quote (car rhs))
                                    (list? rhs)
                                    (= 2 (length rhs))))
                           (hash-set ht id (if (pair? rhs)
                                               (cadr rhs)
                                               rhs)))]
                     [_ #f])))
            => (lambda (ht)
                 ;; This module is so simple that we don't need to `eval` it.
                 (lambda (key [default (lambda () (error 'info.rkt "no info for ~a" key))])
                   (hash-ref ht key default)))]
           [else
            ;; Load the module.
            ;; No need to set a reader-guard, since we checked it
            ;; above (a guard will see other uses of #lang for stuff
            ;; that is required).
            ;; We are, however, trusting that the bytecode form of the
            ;; file (if any) matches the source.
            (let ([ns (or ns (info-namespace))])
              (if (and bootstrap?
                       (parameterize ([current-namespace ns])
                         (not (module-declared? file))))
                  ;; Attach `info' language modules to target namespace, and
                  ;; disable the use of compiled bytecode if it fails; we
                  ;; need a trial namespace to try loading bytecode, since
                  ;; the use of bytecode "sticks" for later attempts.
                  (let ([attach!
                         (lambda (ns)
                           (namespace-attach-module enclosing-ns 'setup/infotab ns)
                           (namespace-attach-module enclosing-ns 'setup/infotab/lang/reader ns)
                           (namespace-attach-module enclosing-ns 'info ns)
                           (namespace-attach-module enclosing-ns '(submod info reader) ns))]
                        [try
                         (lambda (ns)
                           (parameterize ([current-namespace ns])
                             (dynamic-require file '#%info-lookup)))])
                    (define ns-id (namespace-module-registry ns))
                    ((with-handlers ([exn:fail? (lambda (exn)
                                                  ;; Trial namespace is damaged, so uncache:
                                                  (hash-set! trial-namespaces ns-id #f)
                                                  ;; Try again from source:
                                                  (lambda ()
                                                    (attach! ns)
                                                    (parameterize ([use-compiled-file-paths null])
                                                      (try ns))))])
                       ;; To reduce the cost of the trial namespace, try to used a cached
                       ;; one previously generated for the `ns':
                       (define try-ns (or (hash-ref trial-namespaces ns-id #f)
                                          (let ([try-ns (make-base-empty-namespace)])
                                            (attach! try-ns)
                                            try-ns)))
                       (define v (try try-ns))
                       (hash-set! trial-namespaces ns-id try-ns)
                       (namespace-attach-module try-ns file ns)
                       (lambda () v))))
                  ;; Can use compiled bytecode, etc.:
                  (parameterize ([current-namespace ns])
                    (dynamic-require file '#%info-lookup))))])]
         [else (err "does not contain a module of the right shape")])))

(define info-namespace
  ;; To avoid loading modules into the current namespace
  ;; when get-info is called, load info modules in a separate
  ;; namespace.
  (let ([ns-box (make-weak-box #f)])
    (lambda ()
      (or (weak-box-value ns-box)
          (let ([ns (make-base-empty-namespace)])
            (set! ns-box (make-weak-box ns))
            ns)))))

;; Weak map from a namespace registry to a trial-loading namespace for
;; bootstrap mode:
(define trial-namespaces (make-weak-hasheq))

;; directory-record = (make-directory-record nat nat key path (listof symbol))
;; eg: (make-directory-record 1 0 '(lib "mzlib") #"mzlib" '(name))
(define-struct directory-record (maj min spec path syms))

(define-struct table (insert   ; directory-record (listof directory-record)
                               ;  -> (listof directory-record)
                      ht       ; hashtable[symbol -o> directory-record]
                      paths    ; (listof (cons path boolean))
                      )
  #:mutable)

(define preferred-table #f)
(define all-available-table #f)
(define no-planet-table #f)
(define no-user-table #f)

;; reset-relevant-directories-state! : -> void
(define (reset-relevant-directories-state!)
  (set! preferred-table
        (make-table
         (lambda (root-dir i l)
           (if (or root-dir (null? l))
             (cons i l)
             (match-let ([(struct directory-record (my-maj my-min _ _ _)) i]
                         [(struct directory-record (their-maj their-min _ _ _))
                          (car l)])
              (if (or (> my-maj their-maj)
                      (and (= my-maj their-maj) (>= my-min their-min)))
                (list i)
                l))))
         #f #f))
  (define (always root-dir i l) (cons i l))
  (set! all-available-table (make-table always #f #f))
  (set! no-planet-table (make-table always #f #f))
  (set! no-user-table (make-table always #f #f)))

(reset-relevant-directories-state!)

;; populate-table : table -> void
(define (populate-table! t)
  ;; Use the colls ht because a collection might be in multiple
  ;; collection paths, and we only want one
  (define-values (path->main-share-relative
                  main-share-relative->path)
    (make-relativize find-share-dir
                     'share
                     'path->main-share-relative
                     'main-share-relative->path))
  (let ([colls (make-hash)])
    (for ([f+root-dir (reverse (table-paths t))])
      (let ([f (car f+root-dir)]
            [root-dir (cdr f+root-dir)])
        (define-values (path->info-relative
                        info-relative->path)
          (make-relativize (lambda () root-dir)
                           'info
                           'path->info-relative
                           'info-relative->path))
        (when (file-exists? f)
          (for ([i (let ([l (with-input-from-file f read)])
                     (cond [(list? l) l]
                           [(eof-object? l) '()] ;; allow completely empty files
                           [else (error 'find-relevant-directories
                                        "bad info-domain cache file: ~a" f)]))])
            (match i
              [(list (and pathbytes (or (? bytes?) (list (or 'info 'share) (? bytes?) ...)))
                     (list (? symbol? fields) ...)
                     key ;; anything is okay here
                     (? integer? maj)
                     (? integer? min))
               (let ([old-items (hash-ref colls key null)]
                     [new-item
                      (make-directory-record
                       maj min key
                       (if (bytes? pathbytes)
                           (let ([p (bytes->path pathbytes)])
                             (if (and (relative-path? p) root-dir)
                                 ;; `raco setup' doesn't generate relative paths anyway,
                                 ;; but it's ok to support them:
                                 (simplify-path (build-path root-dir p))
                                 p))
                           (if (eq? (car pathbytes) 'info)
                               (info-relative->path pathbytes)
                               (main-share-relative->path pathbytes)))
                       fields)])
                 (hash-set! colls key
                            ((table-insert t) root-dir new-item old-items)))]
              [_ (error 'find-relevant-directories
                        "bad info-domain cache entry: ~e in: ~a" i f)])))))
    ;; For each coll, invert the mapping, adding the col name to the list
    ;; for each sym:
    (for* ([(key vals) colls]
           [val vals])
      (match val
        [(struct directory-record (maj min spec path syms))
         (for ([sym syms])
           (hash-set! (table-ht t) sym
                      (cons val (hash-ref (table-ht t) sym null))))]
        [_ (error 'get-info
                  "Internal error: invalid info-domain value format: ~s" val)]))))

(define (find-relevant-directories syms [key 'preferred])
  (map directory-record-path (find-relevant-directory-records syms key)))

(define (find-relevant-directory-records syms [key 'preferred])
  (define t
    (cond [(eq? key 'preferred) preferred-table]
          [(eq? key 'all-available) all-available-table]
          [(eq? key 'no-planet) no-planet-table]
          [(eq? key 'no-user) no-user-table]
          [else (error 'find-relevant-directories "Invalid key: ~s" key)]))
  ;; A list of (cons cache.rktd-path root-dir-path)
  ;;  If root-dir-path is not #f, then paths in the cache.rktd
  ;;  file are relative to it. #f is used for the planet cache.rktd file.
  (define search-path
    ((if (or (eq? key 'no-planet) 
             (eq? key 'no-user))
         (lambda (a l) l) 
         cons)
     (cons user-infotable #f)
     (append
      (map (lambda (coll)
             (cons (build-path coll "info-domain" "compiled" "cache.rktd")
                   coll))
           (if (eq? key 'no-user)
               (get-main-collects-search-dirs)
               (current-library-collection-paths)))
      (map (lambda (base)
             (cons (build-path base "info-cache.rktd") 
                   base))
           (filter
            values
            (if (eq? key 'no-user)
                (list (find-share-dir))
                (list (find-share-dir) (find-user-share-dir))))))))
  (when t
    (unless (equal? (table-paths t) search-path)
      (set-table-ht! t (make-hasheq))
      (set-table-paths! t search-path)
      (populate-table! t)))
  (let ([unsorted
         (if (= (length syms) 1)
           ;; Simple case: look up in table
           (hash-ref (table-ht t) (car syms) null)
           ;; Use a hash table, because the same collection might work
           ;; for multiple syms
           (let ([result (make-hash)])
             (for* ([sym syms]
                    [c (hash-ref (table-ht t) sym null)])
               (hash-set! result c #t))
             ;; Extract the relevant collections:
             (hash-map result (lambda (k v) k))))])
    (sort unsorted bytes<?
          #:key (lambda (dr) (dir->sort-key (directory-record-path dr)))
          #:cache-keys? #t)))

;; dir->sort-key : path -> bytes
;; extracts the name of the directory, dropping any "."s it finds at the ends.
(define (dir->sort-key path)
  (let-values ([(base name dir?) (split-path path)])
    (if (eq? name 'same) (dir->sort-key base) (path->bytes name))))

(define info? (->* [symbol?] [(-> any/c)] any/c))
(define path-or-string? (lambda (x) (or (path? x) (string? x))))

(provide/contract
 (reset-relevant-directories-state! (-> any))
 (get-info (((listof path-or-string?))
            (#:namespace (or/c namespace? #f) #:bootstrap? any/c)
            . ->* . (or/c info? boolean?)))
 (get-info/full ((path-string?)
                 (#:namespace (or/c namespace? #f) #:bootstrap? any/c)
                 . ->* . (or/c info? boolean?)))
 (find-relevant-directories
  (->* [(listof symbol?)]
       [(or/c 'preferred 'all-available 'no-planet 'no-user)]
       (listof path?)))
 (struct directory-record
         ([maj integer?]
          [min integer?]
          [spec any/c]
          [path path?]
          [syms (listof symbol?)]))
 (find-relevant-directory-records
  (->* [(listof symbol?)]
       [(or/c 'preferred 'all-available 'no-planet 'no-user)]
       (listof directory-record?))))
