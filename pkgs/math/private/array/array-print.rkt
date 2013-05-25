#lang typed/racket/base

;; Defines the custom printer used for array values

(require racket/pretty
         racket/fixnum
         "array-struct.rkt"
         "utils.rkt")

(provide print-array)

;; An array is printed in one of three layouts:
;;   1. one-line     on one line, with " " between elements
;;   2. compact      on multiple lines, with "\n" between elements *except the innermost*
;;   3. multi-line   on multiple lines, with "\n" between elements
(define-type Array-Layout (U 'one-line 'compact 'multi-line))

(: print-array (All (A) ((Array A) Symbol Output-Port (U #t #f 0 1) -> Any)))
;; The logic in `print-array' causes the REPL printer to try printing an array in each layout, and
;; keep the first successful one. An overflowing line means failure.
(define (print-array arr name port mode)
  ;; Called to print array elements; may recur (e.g. printing arrays of arrays)
  ;; We never have to consider the `mode' argument again after defining `recur-print'
  (define recur-print
    (cond [(not mode) display]
          [(integer? mode) (λ: ([p : Any] [port : Output-Port])
                             (print p port mode))]  ; pass the quote depth through
          [else write]))
  
  ;; Width of a line
  (define cols (pretty-print-columns))
  
  ;; The following print procedures are parameterized on a port because they're called to print both
  ;; to `port' and to a tentative pretty-printing port we set up further on
  
  (define: (print-prefix [port : Output-Port]) : Any
    (write-string (format "(~a" name) port))
  
  (define: (print-suffix [port : Output-Port]) : Any
    (write-string ")" port))
  
  (: print-all (Output-Port Array-Layout -> Any))
  (define (print-all port layout)
    ;; Get current column so we can indent new lines at least that far
    (define col (port-next-column port))
    
    (: maybe-print-newline (Integer -> Any))
    ;; Prints " " in one-line layout; a newline and some indentation otherwise
    ;; If in compact layout, this *does not* use `pretty-print-newline'. We don't want to signal a line
    ;; overflow in compact layout unless *an array element* overflows. Otherwise, compact layout would
    ;; "overflow" whenever it printed an array with more than 1 axis.
    (define (maybe-print-newline indent)
      (case layout
        [(one-line)  (write-string " " port)]
        [else  (case layout
                 [(compact)  (write-string "\n" port)]
                 [else  (pretty-print-newline port (assert cols integer?))])
               (write-string (make-string (+ col indent) #\space) port)]))
    ;; Print the constructor name
    (print-prefix port)
    (maybe-print-newline 1)  ; +1 to indent past "("
    ;; Print array elements in nested square brackets, with each level indented an extra space
    (define ds (array-shape arr))
    (define dims (vector-length ds))
    (define proc (unsafe-array-proc arr))
    ;; We mutate this in row-major order instead of creating a new index vector for every element
    (define: js : Indexes (make-vector dims 0))
    ;; For each shape axis
    (let i-loop ([#{i : Nonnegative-Fixnum} 0])
      (cond [(i . fx< . dims)  ; proves i : Index
             (write-string "#[" port)
             (define di (vector-ref ds i))  ; length of axis i
             ;; For each index on this axis
             (let ji-loop ([#{ji : Nonnegative-Fixnum} 0])
               (when (ji . fx< . di)  ; proves ji : Index
                 (vector-set! js i ji)
                 ;; Print either nested elements or the element here
                 (i-loop (fx+ i 1))
                 ;; Print delimiter when not printing the last element on this axis
                 (when (ji . fx< . (fx- di 1))
                   (cond [(and (eq? layout 'compact) (fx= i (fx- dims 1)))
                          ;; Keep elements on one line in compact layout
                          (write-string " " port)]
                         [else
                          ;; +1 to indent past "(", +2 to indent past the first "#[", and `i' axes
                          (maybe-print-newline (+ 3 (* i 2)))]))
                 (ji-loop (fx+ ji 1))))
             (write-string "]" port)]
            [else
             ;; Print an element
             (recur-print (proc js) port)]))
    ;; Print the closing delimiter
    (print-suffix port))
  
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
           ;; Try printing in compact layout
           (print-all tport 'compact)
           ;; If a line overflows, the failure thunk returns past this
           (tentative-pretty-print-port-transfer tport port))]
        [else
         ;; No pretty printer, or printing to infinite-width lines, so print on one line
         (print-all port 'one-line)]))
