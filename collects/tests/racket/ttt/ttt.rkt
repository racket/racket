
(require mzlib/etc mzlib/compat)
(load-relative "listlib.rkt")
(load-relative "veclib.rkt")
(load-relative "tic-func.rkt")

(let loop ()
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  ; (dump-memory-stats)
  (time (tic-tac-toe 1 1))
  '(loop))
