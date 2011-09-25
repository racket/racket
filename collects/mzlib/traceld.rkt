#lang racket/base

(let ([load (current-load)]
      [load-extension (current-load-extension)]
      [ep (current-error-port)]
      [tab ""])
  (let ([mk-chain
         (lambda (load)
           (lambda (filename expected-module)
             (fprintf ep
                      "~aloading ~a at ~a\n" 
                      tab filename (current-process-milliseconds))
             (begin0
              (let ([s tab])
                (dynamic-wind
                    (lambda () (set! tab (string-append " " tab)))
                    (lambda () 
                      (load filename expected-module))
                    (lambda () (set! tab s))))
              (fprintf ep
                       "~adone ~a at ~a\n"
                       tab filename (current-process-milliseconds)))))])
    (current-load (mk-chain load))
    (current-load-extension (mk-chain load-extension))))

