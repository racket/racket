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
                    (fprintf
                     (current-error-port)
                     "warning: tool ~s registered twice: ~e and ~e"
                     (car entry)
                     (car p)
                     d)))
            (hash-set! tools (car entry) entry)]
           [else
            (fprintf
             (current-error-port)
             "warning: ~s provided bad `raco-commands' spec: ~e"
             d
             entry)]))))
    tools))
