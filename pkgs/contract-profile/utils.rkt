#lang racket/base

(require racket/port racket/contract racket/list setup/collects)

(provide (except-out (all-defined-out) shorten-paths))

(struct contract-profile
  (total-time
   ;; (pairof blame? profile-sample)
   ;; samples taken while a contract was running
   live-contract-samples
   ;; (listof blame?)
   ;; all the blames that were observed during sampling
   all-blames
   ;; profile?
   ;; regular time profile
   regular-profile))

(define (samples-time samples)
  (for/sum ([s (in-list samples)])
    (cadr s)))

(define output-file-prefix "tmp-contract-profile-")


;; for testing. don't generate output files
(define dry-run? (make-parameter #f))

(define-syntax-rule (with-output-to-report-file file body ...)
  (if (dry-run?)
      (parameterize ([current-output-port (open-output-nowhere)])
        body ...)
      (with-output-to-file file
        #:exists 'replace
        (lambda () body ...))))

;; for debugging
(define (format-blame b)
  (format (string-append "#<blame positive=~a\n"
                         "        negative=~a\n"
                         "        contract=~a\n"
                         "        value=~a\n"
                         "        source=~a>\n")
          (blame-positive b) (blame-negative b)
          (blame-contract b) (blame-value b) (blame-source b)))

;; (listof (U path-string? submodule-path #f)) -> same
(define (shorten-paths ps*)

  ;; zeroth pass, remove non-paths
  (define ps
    (for/list ([p (in-list ps*)]
               #:when (or (path-string? p)
                          (and (list? p) ; submodule
                               (not (empty? p))
                               (path-string? (first p)))))
      p))

  ;; zeroth.5 pass, chop off submodule parts, to put back later
  (define submodules ; (hashof path (U submodule-part #f))
    (for/hash ([p ps])
      (values p (and (list? p) (rest p)))))

  ;; first pass, convert to collect relative paths if possible
  (define w/o-submodules
    (for/hash ([p ps])
      (values p (path->module-path (if (list? p) (first p) p)))))

  ;; second pass, make non-collect paths relative to their common ancestor
  (define-values (collect-paths non-collect-paths)
    (for/fold ([collect-paths     (hash)]
               [non-collect-paths (hash)])
        ([(k v) (in-hash w/o-submodules)])
      (if (list? v) ; collect path?
          (values (hash-set collect-paths k v)
                  non-collect-paths)
          (values collect-paths
                  (values (hash-set non-collect-paths k v))))))
  (define relative-paths
    (cond
     [(hash-empty? non-collect-paths) ; degenerate case
      (hash)]
     [else
      ;; not using hash-keys and hash-values. need the orders to match
      (define as-list (hash->list non-collect-paths))
      (define origs   (map car as-list))
      (define vs      (map cdr as-list))
      ;; this transformation preserves order of paths
      (define relative
        (let loop ([paths (map explode-path vs)])
          (define head-base (first (first paths)))
          (if (and (for/and ([p (rest paths)]) (equal? (first p) head-base))
                   (not (for/or ([p paths]) (empty? (rest p)))))
              ;; all start with the same directory, drop it
              ;; (and we're not dropping filenames)
              (loop (map rest paths))
              ;; not all the same, we're done
              (for/list ([p paths]) (apply build-path p)))))
      (for/hash ([o (in-list origs)]
                 [v (in-list relative)])
        (values o v))]))

  ;; final pass, reassemble submodule parts
  ;; start with collect paths
  (define init-table
    (for/hash ([(k v) (in-hash collect-paths)])
      (define submodule-part (hash-ref submodules k #f))
      (values k
              (if submodule-part (cons v submodule-part) v))))
  ;; then add non-collect paths
  (for/fold ([table init-table])
      ([(k v) (in-hash relative-paths)])
    (define submodule-part (hash-ref submodules k #f))
    (hash-set table
              k
              (if submodule-part (cons v submodule-part) v))))


;; (sequenceof A) (A -> (U path-string? submodule-path #f)) -> (A -> (U ...))
(define (make-shortener ps* [extract-path values])
  ;; special-case things shorten-paths can't deal with
  ;; these should just map to themselves
  (define-values (ps bad)
    (partition (lambda (p)
                 (or (path-string? p)
                     (and (list? p) ; submodule path
                          (not (empty? p))
                          (path-string? (first p)))))
               ;; can be any kind of sequence, turn into a list
               (for/list ([p ps*]) p)))
  (define extracted (map extract-path ps))
  (define shortened (shorten-paths extracted))
  (define init-table
    (for/hash ([p ps]
               [e extracted])
      (values p (hash-ref shortened e))))
  ;; add bad "paths", mapping to themselves
  (define table
    (for/fold ([table init-table])
        ([b (in-list bad)])
      (hash-set table b b)))
  (lambda (p)
    (or (hash-ref table p #f)
        (extract-path p))))

(define (make-srcloc-shortener srcs [extract-srcloc values])
  (define extracted
    (for/list ([s srcs])
      (srcloc-source (extract-srcloc s))))
  (define shortened (shorten-paths extracted))
  (define table
    (for/hash ([p srcs]
               [e extracted])
      (values p (hash-ref shortened e))))
  (lambda (p)
    (define target (hash-ref table p #f))
    (if target
        (struct-copy srcloc
                     (extract-srcloc p)
                     [source target])
        (extract-srcloc p))))
