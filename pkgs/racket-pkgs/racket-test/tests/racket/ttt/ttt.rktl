
(require mzlib/etc mzlib/compat)
(load-relative "listlib.rktl")
(load-relative "veclib.rktl")
(load-relative "tic-func.rktl")

(let loop ()
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  ; (dump-memory-stats)
  (time (tic-tac-toe 1 1))
  '(loop))
