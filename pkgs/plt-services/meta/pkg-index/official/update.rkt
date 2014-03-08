#lang racket/base
(require racket/list
         racket/function
         pkg/util
         racket/package
         (prefix-in pkg: pkg/lib)
         "common.rkt")

(define (update-all)
  (update-checksums #f (package-list)))
(define (update-pkgs pkgs)
  (update-checksums #t pkgs))

(define (update-checksums force? pkgs)
  (for-each (curry update-checksum force?) pkgs))

(define (update-checksum force? pkg-name)
  (with-handlers
      ([exn:fail?
        (λ (x)
          (define i (package-info pkg-name))
          (package-info-set! 
           pkg-name 
           (hash-set i 'checksum-error (exn-message x))))])
    (define i (package-info pkg-name))
    (define old-checksum
      (package-ref i 'checksum))
    (define now (current-seconds))
    (define last (hash-ref i 'last-checked -inf.0))
    (when (or force?
              (>= (- now last) (* 1 60 60)))
      (printf "\tupdating ~a\n" pkg-name)
      (define new-checksum
        (package-url->checksum
         (package-ref i 'source)
         #:pkg-name pkg-name))
      (package-begin
       (define* i
         (hash-set i 'checksum
                   (or new-checksum
                       old-checksum)))
       (define* i
         (hash-set i 'last-checked now))
       (define* i
         (hash-update i 'versions
                      (λ (v-ht)
                        (for/hash ([(v vi) (in-hash v-ht)])
                          (define old-checksum (hash-ref vi 'checksum ""))
                          (define new-checksum
                            (package-url->checksum
                             (hash-ref vi 'source "")
                             #:pkg-name pkg-name))
                          (values v
                                  (hash-set vi 'checksum
                                            (or new-checksum
                                                old-checksum)))))
                      hash))
       (define* i
         (if (and new-checksum (equal? new-checksum old-checksum)
                  ;; update if 'modules was not present:
                  (hash-ref i 'modules #f))
           i
           (hash-set (update-from-content i) 'last-updated now)))
       (define* i
         (hash-set i 'checksum-error #f))
       (package-info-set! pkg-name i)))))

(define (update-from-content i)
  (define-values (checksum module-paths dependencies)
    (pkg:get-pkg-content (pkg:pkg-desc (hash-ref i 'source)
                                       #f
                                       (hash-ref i 'name)
                                       (hash-ref i 'checksum)
                                       #f)))
  (package-begin
   (define* i (hash-set i 'modules module-paths))
   (define* i (hash-set i 'dependencies dependencies))
   i))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "update"
   #:args pkgs
   (cond
     [(empty? pkgs)
      (update-all)
      (run-static! empty)]
     [else
      (update-pkgs pkgs)
      (run-static! pkgs)])))
