#lang scheme/base

(require scheme/match 
         scheme/contract 
         planet/cachepath 
         syntax/modread
         "path-relativize.rkt")

;; in addition to infodomain/compiled/cache.rktd, getinfo will look in this 
;; file to find mappings. PLaneT uses this to put info about installed
;; planet packages.
(define user-infotable (get-planet-cache-path))

;; get-info : (listof path-or-string) -> info/#f
(define (get-info coll-path #:namespace [ns #f])
  (get-info/full (apply collection-path
                        (map (lambda (x) (if (path? x) (path->string x) x))
                             coll-path))
                 #:namespace ns))

;; HACK:
;; This require is not used.  It just requires the file, since
;; otherwise the reader guard below will be invoked on it too, and
;; that will make it throw up.  One possible solution for this would
;; be for the security guard to be provided with the file in question.
;; Another would be to force all info files to use `#lang' which means
;; that we'll be able to query their module-language via the
;; `get-info' protocol.
(require (prefix-in !!!HACK!!! setup/infotab/lang/reader))

;; get-info/full : path -> info/#f
(define (get-info/full dir #:namespace [ns #f])
  (or (get-info/full/ext dir "rkt" ns)
      (get-info/full/ext dir "ss" ns)))

(define (get-info/full/ext dir ext ns)
  (define file (build-path dir (format "info.~a" ext)))
  (define (err fmt . args)
    (apply error 'get-info (string-append "info file " fmt " in ~a")
           (append args (list file))))
  (define (contents)
    (parameterize ([current-reader-guard
                    (lambda (x)
                      (if (or (eq? x 'setup/infotab/lang/reader)
                              (equal? x '(submod setup/infotab reader)))
                        x
                        (err "has illegal #lang or #reader")))])
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
                    'setup/infotab)
                expr ...)
          ;; No need to set a reader-guard, since we checked it
          ;; above (a guard will see other uses of #lang for stuff
          ;; that is required).
          ;; We are, however, trusting that the bytecode form of the
          ;; file (if any) matches the source.
          (parameterize ([current-namespace (or ns (info-namespace))])
            (dynamic-require file '#%info-lookup))]
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

;; reset-relevant-directories-state! : -> void
(define (reset-relevant-directories-state!)
  (set! preferred-table
        (make-table
         (lambda (i l)
           (if (null? l)
             (list i)
             (match-let ([(struct directory-record (my-maj my-min _ _ _)) i]
                         [(struct directory-record (their-maj their-min _ _ _))
                          (car l)])
              (if (or (> my-maj their-maj)
                      (and (= my-maj their-maj) (>= my-min their-min)))
                (list i)
                l))))
         #f #f))
  (set! all-available-table (make-table cons #f #f))
  (set! no-planet-table (make-table cons #f #f)))

(reset-relevant-directories-state!)

;; populate-table : table -> void
(define (populate-table! t)
  ;; Use the colls ht because a collection might be in multiple
  ;; collection paths, and we only want one
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
              [(list (and pathbytes (or (? bytes?) (list 'info (? bytes?) ...)))
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
                                 (build-path root-dir p)
                                 p))
                           (info-relative->path pathbytes))
                       fields)])
                 (hash-set! colls key
                            ((table-insert t) new-item old-items)))]
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
          [else (error 'find-relevant-directories "Invalid key: ~s" key)]))
  ;; A list of (cons cache.rktd-path root-dir-path)
  ;;  If root-dir-path is not #f, then paths in the cache.rktd
  ;;  file are relative to it. #f is used for the planet cache.rktd file.
  (define search-path
    ((if (eq? key 'no-planet) (lambda (a l) l) cons)
     (cons user-infotable #f)
     (map (lambda (coll)
            (cons (build-path coll "info-domain" "compiled" "cache.rktd")
                  coll))
          (current-library-collection-paths))))
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
 (get-info (((listof path-or-string?)) (#:namespace (or/c namespace? #f)) . ->* . (or/c info? boolean?)))
 (get-info/full ((path-string?) (#:namespace (or/c namespace? #f)) . ->* . (or/c info? boolean?)))
 (find-relevant-directories
  (->* [(listof symbol?)]
       [(lambda (x) (memq x '(preferred all-available no-planet)))]
       (listof path?)))
 (struct directory-record
         ([maj integer?]
          [min integer?]
          [spec any/c]
          [path path?]
          [syms (listof symbol?)]))
 (find-relevant-directory-records
  (->* [(listof symbol?)]
       [(or/c 'preferred 'all-available 'no-planet)]
       (listof directory-record?))))
