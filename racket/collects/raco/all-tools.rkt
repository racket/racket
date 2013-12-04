#lang racket/base
(require setup/getinfo
         racket/list)

(provide all-tools)

(define (get-info/full/skip dir)
  (with-handlers ([exn:fail? (lambda (exn)
                               (log-error (exn-message exn))
                               #f)])
    (get-info/full dir)))

(define (all-tools)
  (let* ([dirs (find-relevant-directories '(raco-commands) 'all-available)]
         [tools (make-hash)])
    (for ([i (in-list (filter-map get-info/full/skip dirs))]
          [d (in-list dirs)])
      (let ([entries (let ([l (if i
                                  (i 'raco-commands (lambda () null))
                                  null)])
                       (if (list? l)
                           l
                           (list l)))])
        (for ([entry (in-list entries)])
          (cond
           [(and (list? entry)
                 (= (length entry) 4)
                 (string? (car entry))
                 (module-path? (cadr entry))
                 (string? (caddr entry))
                 (or (not (list-ref entry 3))
                     (real? (list-ref entry 3))))
            (let ([p (hash-ref tools (car entry) #f)])
              (when p
                    (eprintf
                     "warning: tool ~s registered twice: ~e and ~e\n"
                     (car entry)
                     (car p)
                     d)))
            (let ([entry (let ([e (cadr entry)])
                           (if (or (string? e)
                                   (and (pair? e)
                                        (eq? (car e) 'file)
                                        (relative-path? (cadr e))))
                               ;; convert absolute path to realive to "info.rkt":
                               (list* (car entry)
                                      (build-path d (if (pair? e)
                                                        (cadr e)
                                                        e))
                                      (cddr entry))
                               ;; module path is absolute already:
                               entry))])
              (hash-set! tools (car entry) entry))]
           [else
            (eprintf "warning: ~s provided bad `raco-commands' spec: ~e\n"
                     d
                     entry)]))))
    tools))
