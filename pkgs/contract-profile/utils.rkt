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
  ;; zeroth pass, chop off submodule parts, to put back later
  (define ps ; remove non-paths
    (for/list ([p (in-list ps*)]
               #:when (or (path-string? p)
                          (and (list? p) ; submodule
                               (not (empty? p))
                               (path-string? (first p)))))
      p))
  (define submodules ; (listof (U submodule-part #f))
    (for/list ([p ps])
      (and (list? p) (rest p))))
  (define w/o-submodules
    (for/list ([p ps])
      (if (list? p) (first p) p)))
  ;; first pass, convert to collect relative paths if possible
  (define first-pass (map path->module-path w/o-submodules))
  ;; second pass, make non-collect paths relative to their common ancestor
  (define-values (collect-paths non-collect-paths)
    (partition list? first-pass))
  (define relative-paths
    (if (empty? non-collect-paths) ; degenerate case
        '()
        (let loop ([paths (map explode-path non-collect-paths)])
          (define head-base (first (first paths)))
          (if (and (for/and ([p (rest paths)]) (equal? (first p) head-base))
                   (not (for/or ([p paths]) (empty? (rest p)))))
              ;; all start with the same directory, drop it
              ;; (and we're not dropping filenames)
              (loop (map rest paths))
              ;; not all the same, we're done
              (for/list ([p paths]) (apply build-path p))))))
  ;; reassemble submodule parts
  (for/list ([s (in-list submodules)]
             [m (in-list (append collect-paths relative-paths))])
    (if s (cons m s) m)))

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
  (define init-table
    (for/hash ([p ps]
               [s (shorten-paths (map extract-path ps))])
      (values p s)))
  ;; add bad "paths", mapping to themselves
  (define table
    (for/fold ([table init-table])
        ([b (in-list bad)])
      (hash-set table b b)))
  (lambda (p)
    (or (hash-ref table p #f)
        (extract-path p))))

(define (make-srcloc-shortener srcs [extract-srcloc values])
  (define table
    (for/hash ([p srcs]
               [s (shorten-paths (for/list ([s srcs])
                                   (srcloc-source (extract-srcloc s))))])
      (values p s)))
  (lambda (p)
    (define target (hash-ref table p #f))
    (if target
        (struct-copy srcloc
                     (extract-srcloc p)
                     [source target])
        (extract-srcloc p))))
