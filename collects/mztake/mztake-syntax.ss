#|
(mztake-process p ("sine.ss" [sin/x 5 8 bind '(sin-x x)]
                            [foo 10 20 bind '(sin-x x)])
                 ("sine-extra.ss"))
 
(define sin/x (hold sin/x))
(define x (+ 200 (second sin/x)))
(print-b "x:" x)

*** translates to ***

(define p (create-debug-process))
(define-values (sin/x foo ...)(
 (let ([tmp (create-debug-client p "sine.ss")])
   (values (create-trace tmp 5 8 'bind '(sin-x x))))
...
|#

(module mztake-syntax (lib "frtime-big.ss" "frtime")
  
  (require (lib "mztake.ss" "mztake")
           (lib "useful-code.ss" "mztake/private"))
  
  (define-syntax mztake-process
    (syntax-rules (mztake-process)
      [(mztake-process proc-id (client (trace line col cmd . args) ...) ...)
       (begin
         (define proc-id (create-debug-process))
         (begin
           (define-values (trace ...)
             (let ([tmp (create-debug-client proc-id 'client)])
               (values
                (create-trace tmp line col 'cmd . args)
                ...))) ...))]))
  
  (provide mztake-process
           (all-from-except (lib "frtime-big.ss" "frtime") #%module-begin)
           (all-from (lib "mztake.ss" "mztake"))
           (all-from (lib "useful-code.ss" "mztake/private"))))