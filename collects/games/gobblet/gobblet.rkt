#lang mzscheme
(require mzlib/unitsig
         (only mzlib/unit unit import export)
         mzlib/file
         mred
         "sig.rkt"
         "model.rkt"
         "gui.rkt"
         "heuristics.rkt"
         "explore.rkt"
         "../show-scribbling.rkt")

(provide game@)

(define game@
  (unit (import) (export)

    (define (make-gobblet-unit size)
      (compound-unit/sig
       (import)
       (link [CONFIG : config^
              ((unit/sig config^ (import)
                 (define BOARD-SIZE size)))]
             [RESTART : restart^
              ((unit/sig restart^ (import)
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
                        (help)))))))]
             [MODEL : model^ (model-unit CONFIG)]
             [HEURISTICS : heuristics^ (heuristics-unit CONFIG MODEL EXPLORE)]
             [EXPLORE : explore^ (explore-unit CONFIG MODEL)]
             [GUI : () (gui-unit CONFIG MODEL RESTART HEURISTICS EXPLORE)])
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
         (lambda () (invoke-unit/sig (make-gobblet-unit board-size))))))

    (start-gobblet (get-preference 'gobblet:board-size (lambda () 3)))))
