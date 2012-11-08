#lang racket/base
(require racket/list
         racket/port
         racket/file
         racket/contract
         setup/unpack)

;; After PR12904 is fixed, hopefully I won't need this.

(define (unplt pkg pkg-dir)
  (define (path-descriptor->path pd)
    (if (or (eq? 'same pd)
            (path? pd))
      pd
      (second pd)))
  (define (write-file file* content-p)
    (define file (path-descriptor->path file*))
    #;(printf "\twriting ~a\n" file)
    (with-output-to-file
        (build-path pkg-dir file)
      (λ () (copy-port content-p (current-output-port)))))

  (fold-plt-archive pkg
                    void
                    void
                    (λ (dir* _a)
                      (define dir (path-descriptor->path dir*))
                      #;(printf "\tmaking ~a\n" dir)
                      (define new-dir
                        (build-path pkg-dir
                                    dir))
                      (unless (or (equal? (build-path 'same)
                                          dir)
                                  (directory-exists? new-dir))
                        (make-directory* new-dir)))
                    (case-lambda
                      [(file content-p _a)
                       (write-file file content-p)]
                      [(file content-p _m _a)
                       (write-file file content-p)])
                    (void)))

(provide
 (contract-out
  [unplt (-> path-string? path-string?
             void?)]))
