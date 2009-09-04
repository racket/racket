#lang scheme/base
(require scheme/unit
         scheme/runtime-path
         scheme/gui/base
         scheme/class
         "../show-scribbling.ss"
         string-constants/string-constant
         (prefix-in x: lang/htdp-intermediate-lambda)
         (prefix-in y: htdp/world))

(provide game@)
(define orig-namespace (current-namespace))
(define-runtime-path chat-noir "chat-noir-literate.ss")

(define-unit game@
  (import)
  (export)
  (define ns (make-base-namespace))
  
  ;; a hack.
  ;; this adds a help button to the world.ss window
  (thread
   (λ ()
     (let loop ([n 0])
       (when (n . < . 100)
         (sleep 1/10)
         (let ([fs (get-top-level-windows)])
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
                     [label (string-constant help)]))]))))))
  
  (parameterize ([current-namespace ns])
    (namespace-attach-module orig-namespace '(lib "mred.ss" "mred"))
    (namespace-attach-module orig-namespace '(lib "class.ss" "scheme"))
    (dynamic-require chat-noir #f)))
