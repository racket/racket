#|
(debugger
 (processes (p ["sine.ss" (sin/x 5 8 bind '(sin-x x)))
                          (foo 10 20 bind '(sin-x x)))]
               ["sine-extra.ss"]))
 
 (define sin/x (hold sin/x))
 (define x (+ 200 (second sin/x)))
 (print-b "x:" x))

*** translates to ***

(define p (create-debug-process))
(define <temp2> (create-debug-client p "sine.ss"))
(define sin/x (trace/bind <temp2> 5 8 '(sin-x x)))
...
(start/resume <temp1>)
...
|#

(module mztake-syntax (lib "frtime-big.ss" "frtime")
  
  (require (lib "mztake.ss" "mztake"))
  (require-for-syntax (lib "list.ss"))
  
  (define-syntax (debugger stx)
    (syntax-case stx (processes)
      [(debugger
        (processes (clause ...))
        expr ...)
       (foldl
        (lambda (cls prev)
          (syntax-case prev ()
            [(begin transformed-expr ...)
             (syntax-case cls ()
               [(proc-id (client trace-clause ...) ...)
                (with-syntax ([(client-name ...)
                               (generate-temporaries #'(client ...))])
                  (begin
                    transformed-expr ...
                    (define proc-id (create-debug-process))
                    (define-values (client-name trace ...)
                      (let ([tmp (create-debug-client proc-id)])
                        ) ...
                    
         )