#|
(debugger
 (processes (p ("sine.ss" [sin/x 5 8 bind '(sin-x x)]
                          [foo 10 20 bind '(sin-x x)])
               ("sine-extra.ss")))
 
 (define sin/x (hold sin/x))
 (define x (+ 200 (second sin/x)))
 (print-b "x:" x))

*** translates to ***

(define p (create-debug-process))
(define-values (sin/x foo ...)(
 (let ([tmp (create-debug-client p "sine.ss")])
   (values (create-trace tmp 5 8 'bind '(sin-x x))))
...
(start/resume <temp1>)
...
|#

(module mztake-syntax (lib "frtime-big.ss" "frtime")
  
  (define-syntax debugger-module-begin
    (syntax-rules (debugger)
      [(_ (debugger . clauses))
       (#%module-begin (debugger . clauses))]))
  
  (define-syntax debugger
    (syntax-rules (processes)
      [(_ (processes (proc-id (client (trace line col cmd . args) ...))
                     ...)
          expr ...)
       (begin
         (define proc-id (create-debug-process))
         ...
         (define-values (trace ...)
           (let ([tmp (create-debug-client proc-id client)])
             (values
              (create-trace line col 'cmd . args)
              ...)))
         ...
         expr
         ...)]))
  
  (provide debugger
           (rename debugger-module-begin #%module-begin)
           (all-from-except (lib "frtime-big.ss" "frtime") #%module-begin)))
