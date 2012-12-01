#lang racket/base
(require planet/util
         rackunit
         racket/port)

(define debug? #t)

(define (install-one package-spec key)
  (define op (open-output-string))
  (parameterize ([current-output-port op]
                 [current-namespace (make-base-namespace)])
    (dynamic-require package-spec #f))
  (unless (regexp-match #rx"working properly" (get-output-string op))
    (error 'install-one "installation failed; key ~s" key)))

(define (find-test-connection-dir package-spec)
  (define-values (base name dir?)
    (split-path
     (resolved-module-path-name
      ((current-module-name-resolver) 
       package-spec 
       #f #f #f))))
  (define-values (base2 name2 dir?2)
    (split-path base))
  base2)

(define (dir-tree-and-sizes path)
  (let loop ([path path]
             [inside-compiled? #f])
    (define-values (base name dir?) (split-path path))
    (define s-name (path->string name))
    (cond
      [(directory-exists? path)
       (cons s-name
             (map (λ (x) (loop (build-path path x) 
                               (or inside-compiled?
                                   (equal? "compiled" s-name))))
                  (directory-list path)))]
      [(file-exists? path)
       (list s-name (if inside-compiled?
                        'ignore-sizes-inside-compiled-dirs
                        (file-size path)))]
      [else
       (list s-name #f)])))


(define lr (make-log-receiver (current-logger) 'info))
(define docs-build-chan (make-channel))

;; get-docs-build-count : -> number
;; effect: aborts the loop that watches the docs build counting
(define (get-docs-build-count)
  (define new-chan (make-channel))
  (channel-put docs-build-chan new-chan)
  (channel-get new-chan))
(void
 (thread
  (λ ()
    (let loop ([num 0])
      (sync
       (handle-evt
        lr
        (λ (vec)
          (when debug?
            (printf "~a\n" (vector-ref vec 1)))
          (loop
           (if (regexp-match #rx"raco setup: --- building documentation ---"
                             (vector-ref vec 1))
               (+ num 1)
               num))))
       (handle-evt
        docs-build-chan
        (λ (return)
          (channel-put return num))))))))

(let ([package-spec '(planet "test-connection-mzscheme.scm" ("planet" "test-connection.plt" 1 (= 0)))])
  (printf "installing for the first time\n")
  (install-one package-spec 'seq1)
  (define test-connection-dir (find-test-connection-dir package-spec))
  (define non-parallel-install-sizes (dir-tree-and-sizes test-connection-dir))
  (printf "removing the first one\n")
  
  (parameterize ([current-output-port (if debug?
                                          (current-output-port)
                                          (open-output-nowhere))])
    (remove-pkg "planet" "test-connection.plt" 1 0))

  (printf "installing in parallel\n")
  (define thds
    (for/list ([x (in-range 0 10)])
      (thread (λ () (install-one package-spec 'par1)))))
  (for ([thd (in-list thds)])
    (thread-wait thd))
  
  (define parallel-install-sizes (dir-tree-and-sizes test-connection-dir))
  
  (check-equal? parallel-install-sizes
                non-parallel-install-sizes)
  
  (printf "removing the parallel one\n")
  (parameterize ([current-output-port (if debug?
                                          (current-output-port)
                                          (open-output-nowhere))])
    (remove-pkg "planet" "test-connection.plt" 1 0))
  
  (check-equal? (get-docs-build-count)
                6))

