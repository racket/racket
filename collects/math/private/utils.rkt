#lang typed/racket/base

(require racket/pretty
         racket/fixnum
         racket/flonum
         "exception.rkt")

(provide pretty-print-constructor
         check-flvector-lengths!)

(: port-next-column (Output-Port -> Natural))
;; Helper to avoid the annoying #f column value
(define (port-next-column port)
  (define-values (_line col _pos) (port-next-location port))
  (if col col 0))

(define-type Constructor-Layout (U 'one-line 'multi-line))

(: pretty-print-constructor (Symbol (Listof Any) Output-Port (U #t #f 0 1) -> Any))
(define (pretty-print-constructor name args port mode)
  ;; Called to print arguments; may recur (e.g. printing constructed arguments)
  ;; We never have to consider the `mode' argument again after defining `recur-print'
  (define recur-print
    (cond [(not mode) display]
          [(integer? mode) (λ: ([p : Any] [port : Output-Port])
                             (print p port mode))]  ; pass the quote depth through
          [else write]))
  
  (define cols (pretty-print-columns))
  
  (: print-all (Output-Port Constructor-Layout -> Any))
  (define (print-all port layout)
    ;; Get current column so we can indent new lines at least that far
    (define col (port-next-column port))
    ;; Print the constructor name
    (write-string (format "(~a" name) port)
    (for ([arg  (in-list args)])
      (case layout
        [(one-line)  (write-string " " port)]
        [else  (pretty-print-newline port (assert cols integer?))
               (write-string (make-string (+ col 1) #\space) port)])
      (recur-print arg port))
    (write-string ")" port))
  
  ;; See what the printer has in mind for us this time
  (cond [(and (pretty-printing) (integer? cols))
         ;; Line-width-constrained pretty-printing: woo woo!
         (let/ec: return : Any  ; used as a return statement
           ;; Wrap the port with a tentative one, in case compact layout overflows lines
           (define: tport : Output-Port
             (make-tentative-pretty-print-output-port
              port
              (max 0 (- cols 1))  ; width: make sure there's room for the closing delimiter
              (λ ()  ; failure thunk
                ;; Reset accumulated graph state
                (tentative-pretty-print-port-cancel (assert tport output-port?))
                ;; Compact layout failed, so print in multi-line layout
                (return (print-all port 'multi-line)))))
           ;; Try printing on one line
           (print-all tport 'one-line)
           ;; If a line overflows, the failure thunk returns past this
           (tentative-pretty-print-port-transfer tport port))]
        [else
         ;; No pretty printer, or printing to infinite-width lines, so print on one line
         (print-all port 'one-line)]))

(: check-flvector-lengths! (Symbol Index FlVector * -> Void))
(define (check-flvector-lengths! name n . xss)
  (for: ([xs  (in-list xss)])
    (unless (fx= n (flvector-length xs))
      (raise-length-error name "FlVector" xs n))))
