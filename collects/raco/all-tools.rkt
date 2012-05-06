#lang scheme/base

(require setup/getinfo)
(provide all-tools)

(define (all-tools)
  (let* ([dirs (find-relevant-directories '(raco-commands))]
         [tools (make-hash)])
    (for ([i (in-list (map get-info/full dirs))]
          [d (in-list dirs)])
      (let ([entries (let ([l (i 'raco-commands (lambda () null))])
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
