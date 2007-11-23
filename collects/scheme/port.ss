
(module port scheme/base
  (require mzlib/port)
  (provide (except-out (all-from-out mzlib/port)
                       strip-shell-command-start)))
