#lang racket/gui

(require setup/getinfo "show-help.rkt")

(define-struct game (file name set icon))

(define (get-game gamedir)
  (define-values (base name dir?) (split-path gamedir))
  (define game (path-element->string name))
  (define info (with-handlers ([exn:fail? (lambda (x) #f)])
                 (get-info (list "games" game))))
  (define main (and info (info 'game (lambda () #f))))
  (define (gamefile f) (build-path gamedir f))
  (and main
       (make-game
        (gamefile main)
        (info 'name (λ () (string-titlecase (regexp-replace* #rx"-" game " "))))
        (info 'game-set  (λ () "Other Games"))
        (info 'game-icon (λ () (gamefile (format "~a.png" game)))))))

(define (run-game game)
  (define c (make-custodian))
  (define run
    (dynamic-wind
      begin-busy-cursor
      (lambda ()
        (with-handlers ([exn? (lambda (e) (lambda () (raise e)))])
          (let ([u (dynamic-require (game-file game) 'game@)])
            (lambda () (invoke-unit u)))))
      end-busy-cursor))
  (parameterize* ([current-custodian c]
                  [current-namespace (make-gui-empty-namespace)]
                  [current-eventspace (make-eventspace)])
    (queue-callback
     (lambda ()
       (exit-handler (lambda (v) (custodian-shutdown-all c)))
       (with-handlers ([exn? (lambda (e)
                               (message-box (format "Error in \"~a\""
                                                    (game-name game))
                                            (let ([ep (open-output-string)])
                                              (parameterize ([current-error-port ep])
                                                ((error-display-handler) (exn-message e) e))
                                              (get-output-string ep))
                                            f
                                            '(ok)))])
         (run))))))

(define games
  (for/list ([gamedir (in-list (find-relevant-directories '(game)))])
    (get-game gamedir)))

(define game-sets
  (let ([ht (make-hash)])
    (for ([g (in-list games)])
      (let ([set (game-set g)])
        (hash-set! ht set (cons g (hash-ref ht set '())))))
    (sort (hash-map ht cons)
          (lambda (x y)
            (let ([xlen (length x)] [ylen (length y)])
              (cond [(> xlen ylen) #t]
                    [(< xlen ylen) #f]
                    [else (string<? (car x) (car y))]))))))

(define f (new (class frame%
                 (augment* [on-close (lambda () (exit))])
                 (super-new))
               [label "PLT Games"]
               [style '(metal no-resize-border)]))
(define main (make-object horizontal-panel% f))
(send f set-alignment 'left 'top)
(send f stretchable-width #f)
(send f stretchable-height #f)

(for ([set (in-list game-sets)])
  (define set-name (car set))
  (define games (cdr set))
  (define panel
    (new group-box-panel% [label set-name] [parent main]))
  (define buttons
    (map (lambda (game)
           (new button%
                [label (list (read-bitmap (game-icon game)) (game-name game) 'left)]
                [parent panel]
                [callback (lambda _ (run-game game))]))
         games))
  (define sorted
    (sort buttons (lambda (x y) (< (send x min-width) (send y min-width)))))
  (send panel change-children (lambda (l) sorted)))

(define show-games-help (show-help '("games") "About PLT Games"))

(application-about-handler show-games-help)
(application-preferences-handler
 (lambda ()
   (message-box
    "Oops"
    "There aren't actually any preferences."
    f
    '(ok))))

(send f show #t)
