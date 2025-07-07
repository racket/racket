#lang racket/base
(require pkg/lib
         setup/getinfo)

(define cache (make-hash))
(define metadata-ns (make-base-namespace))

(let loop ([pkgs (installed-pkg-names #:scope 'installation)]
           [seen (hash "racket" #t)])
  (unless (null? pkgs)
    (define pkg (car pkgs))
    (cond
      [(hash-ref seen pkg #f)
       (loop (cdr pkgs) seen)]
      [else
       (define new-seen (hash-set seen pkg #t))
       (define dir (pkg-directory pkg #:cache cache))
       (cond
         [(not dir)
          (printf "~a\n" pkg)
          (loop (cdr pkgs) new-seen)]
         [else
          (define info (get-info/full dir
                                      #:namespace metadata-ns))
          (define deps (extract-pkg-dependencies info #:filter? #t))
          (loop (append deps pkgs) new-seen)])])))
