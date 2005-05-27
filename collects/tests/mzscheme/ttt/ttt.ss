
(require (lib "etc.ss") (lib "compat.ss"))
(load-relative "listlib.ss")
(load-relative "veclib.ss")
(load-relative "tic-func.ss")

(let loop ()
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  ; (dump-memory-stats)
  (time (tic-tac-toe 1 1))
  '(loop))
