#lang racket/base
(require racket/pretty)

(provide pretty-format/write
         pretty-format/display
         pretty-format/print)

(define ((pretty-format/pretty f) v [columns (pretty-print-columns)])
  (parameterize ([current-output-port (open-output-string)]
                 [pretty-print-columns columns])
    (f v)
    (get-output-string (current-output-port))))

(define pretty-format/write (pretty-format/pretty pretty-write))
(define pretty-format/display (pretty-format/pretty pretty-display))
(define pretty-format/print (pretty-format/pretty pretty-print))

;; Ryan: There doesn't seem to be any 'format' going on. Perhaps call
;; them pretty-write-to-string, etc?
;; Bleh, just saw pretty-format in racket/pretty :(


;; by stamourv

(provide break-lines)

;; Takes a string, and breaks it into lines.
(define (break-lines s [columns (pretty-print-columns)])
  (define res (open-output-string))
  (for/fold ([len 0])
      ([word (in-list (regexp-split #px"[[:blank:]]+" s))])
    (let ([new-len (+ len (string-length word) 1)])
      (cond [(< new-len columns)
             (display (format "~a~a" (if (= len 0) "" " ") word) res)
             new-len]
            [else ; break the line
             (display (format "\n~a" word) res)
             (string-length word)])))
  (get-output-string res))
