#lang scheme/base
(require scheme/unit
         scheme/runtime-path
         scheme/gui/base
         scheme/class
         "../show-scribbling.rkt"
         string-constants/string-constant
         (prefix-in x: lang/htdp-intermediate-lambda)
         (prefix-in y: htdp/world))

(provide game@)
(define orig-namespace (current-namespace))
(define-runtime-path chat-noir "chat-noir-literate.rkt")

(define-unit game@
  (import)
  (export)
  
  (define sub-custodian (make-custodian))
  (define main-custodian (current-custodian))
  
  (define (find-windows)
    (let loop ([cust sub-custodian])
      (let o-loop ([objs (custodian-managed-list cust main-custodian)])
        (cond
          [(null? objs) null]
          [else 
           (let ([obj (car objs)])
             (cond
              [(custodian? obj)
               (append (loop obj)
                       (o-loop (cdr objs)))]
              [(eventspace? obj)
               (append (parameterize ([current-eventspace obj])
                         (get-top-level-windows))
                       (o-loop (cdr objs)))]
              [else
               (o-loop (cdr objs))]))]))))
  
  ;; a hack.
  ;; this adds a help button to the world.ss window
  (thread
   (λ ()
     (let loop ([n 0])
       (cond
         [(n . < . 100)
          (sleep 1/10)
          (let ([fs (find-windows)])
            (cond
              [(null? fs)
               (loop (+ n 1))]
              [else
               (let ([f (car fs)]
                     [show-help
                      (show-scribbling
                       '(lib "games/scribblings/games.scrbl")
                       "chat-noir")])
                 (new button% 
                      [parent f]
                      [callback (λ (x y) (show-help))]
                      [label (string-constant help)]))]))]
         [else
          (fprintf (current-error-port) "never found a window\n")]))))
  
  
  ;; start up the game
  
  (parameterize ([current-custodian sub-custodian])
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-attach-module orig-namespace '(lib "mred.rkt" "mred"))
      (namespace-attach-module orig-namespace '(lib "class.rkt" "scheme"))
      ((dynamic-require chat-noir 'main)))))

