#lang racket
(require racket/gui
         "sig.rkt"
         "model.rkt"
         "gui.rkt"
         "heuristics.rkt"
         "explore.rkt"
         "../show-scribbling.rkt")

(provide game@)



(define-unit game@
  (import) 
  (export)
  
  (define (make-gobblet-unit size)
    (define-unit board-config@
      (import) 
      (export config^)
      (define BOARD-SIZE size))
    
    (define-unit restart-unit@
      (import)
      (export restart^)
      (define (new-game n)
        (put-preferences '(gobblet:board-size) (list n) void)
        (parameterize ([current-eventspace orig-eventspace])
          (queue-callback
           (lambda ()
             (start-gobblet n)))))
      (define (show-gobblet-help)
        (parameterize ([current-eventspace orig-eventspace])
          (queue-callback
           (lambda ()
             (unless help
               (set! help (show-scribbling '(lib "games/scribblings/games.scrbl")
                                           "gobblet")))
             (help))))))
    
    (compound-unit/infer
     (import)
     (link [((CONFIG : config^)) board-config@]
           [((RESTART : restart^)) restart-unit@]
           [((MODEL : model^)) model-unit@]
           [((HEURISTICS : heuristics^)) heuristics-unit@]
           [((EXPLORE : explore^)) explore-unit@]
           [() gui-unit@])
     (export)))
  
  (define help #f)
  
  (define orig-eventspace (current-eventspace))
  
  (define (start-gobblet board-size)
    ;; Start a new game as a child process:
    (parameterize* ([current-custodian (make-custodian)]
                    [exit-handler
                     (lambda (v)
                       (custodian-shutdown-all (current-custodian)))]
                    [current-eventspace (make-eventspace)])
                   (queue-callback
                    (lambda () (invoke-unit (make-gobblet-unit board-size))))))
  
  (start-gobblet (get-preference 'gobblet:board-size (lambda () 3))))
