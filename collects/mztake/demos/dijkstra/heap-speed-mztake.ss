(require (lib "mztake.ss" "mztake"))
(set-main! '(lib "heap.ss" "frtime"))

(define start (current-milliseconds))
(set-running! #t)
(exited?)
(- (hold (map-e (lambda (e) (current-milliseconds))
                (changes (exited?)))) start)

