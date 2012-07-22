#lang typed/racket/base

;; The custom printer used for both strict-array and lazy-array struct types

(require racket/pretty
         racket/string
         "array-struct.rkt"
         "utils.rkt")

(provide print-array)

(: print-array (All (A) ((Array A) Output-Port (U #t #f 0 1) -> Any)))
;; Outputs an array using the given mode (write, display, or print with quote depth 0 or 1); uses
;; as many lines as needed to not overflow lines
(define (print-array arr port mode)
  ;; Called to print array elements; may recur (e.g. printing arrays of arrays)
  ;; We never have to consider the `mode' argument again after defining `recur-print'
  (define recur-print
    (cond [(not mode) display]
          [(integer? mode) (λ: ([p : Any] [port : Output-Port])
                             (print p port mode))]  ; pass the quote depth through
          [else write]))
  ;; Actually print the array
  (do-print-array (array-lazy arr)  ; lazy arrays are easy to ref elements from
                  port
                  recur-print
                  (if (lazy-array? arr) 'lazy-array 'strict-array)))

;; An array is printed in one of three layouts:
;;   1. one-line     on one line, with " " between elements
;;   2. compact      on multiple lines, with "\n" between elements *except the innermost*
;;   3. multi-line   on multiple lines, with "\n" between elements
;; The logic in `print-lazy-array' causes the REPL printer to try printing every array in those
;; layouts, in that order. If a line overflows, it tries the next layout.
(define-type Array-Layout (U 'one-line 'compact 'multi-line))

(: do-print-array (All (A) ((lazy-array A) Output-Port (Any Output-Port -> Any) Symbol -> Any)))
(define (do-print-array arr port recur-print struct-name)
  ;; Width of a line
  (define cols (pretty-print-columns))
  
  ;; The following print procedures are parameterized on a port because they're called to print both
  ;; to `port' and to a tentative pretty-printing port we set up further on
  
  (define: (print-prefix [port : Output-Port]) : Any
    (write-string (format "#<~a" struct-name) port))
  
  (define: (print-shape [port : Output-Port]) : Any
    (write-string "(" port)
    (write-string (string-join (map number->string (array-shape arr)) " ") port)
    (write-string ")" port))
  
  (define: (print-suffix [port : Output-Port]) : Any
    (write-string ">" port))
  
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
                 [else  (with-asserts ([cols integer?])
                          (pretty-print-newline port cols))])
               (write-string (make-string (+ col indent) #\space) port)]))
    ;; Print the name and shape
    (print-prefix port)
    (write-string " " port)
    (print-shape port)
    (maybe-print-newline 2)  ; +2 for indenting past "#<"
    ;; Print array elements in nested square brackets, with each level indented an extra space
    (define ds (unsafe-array-shape arr))
    (define dims (vector-length ds))
    (define proc (unsafe-array-proc arr))
    ;; We mutate this in row-major order instead of creating a new index vector for every element
    (define: js : (Vectorof Index) (make-vector dims 0))
    ;; For each shape axis
    (let i-loop ([#{i : Nonnegative-Fixnum} 0])
      (cond [(i . < . dims)  ; proves i : Index
             (write-string "[" port)
             (define di (vector-ref ds i))  ; length of axis i
             ;; For each index on this axis
             (let ji-loop ([#{ji : Nonnegative-Fixnum} 0])
               (when (ji . < . di)  ; proves ji : Index
                 (vector-set! js i ji)
                 ;; Print either nested elements or the element here
                 (i-loop (+ i 1))
                 ;; Print delimiter when not printing the last element on this axis
                 (when (ji . < . (- di 1))
                   (cond [(and (eq? layout 'compact) (= i (- dims 1)))
                          ;; Keep elements on one line in compact layout
                          (write-string " " port)]
                         [else
                          ;; +2 for indenting past "#<", +1 for indenting past the first "[", and
                          ;; +i for each axis
                          (maybe-print-newline (+ 3 i))]))
                 (ji-loop (+ ji 1))))
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
                (with-asserts ([tport output-port?])  ; may be Undefined
                  ;; Reset accumulated graph state
                  (tentative-pretty-print-port-cancel tport))
                ;; Compact layout failed, so print in multi-line layout
                (return (print-all port 'multi-line)))))
           ;; Try printing in compact layout
           (print-all tport 'compact)
           ;; If a line overflows, the failure thunk returns past this
           (tentative-pretty-print-port-transfer tport port))]
        [else
         ;; No pretty printer, or printing to infinite-width lines, so print on one line
         (print-all port 'one-line)]))
