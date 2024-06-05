#lang racket/base

(provide (struct-out run))

(struct run (path/submod phase linkl meta-linkl
                         uses         ; (list (cons path/submod phase-level) ...) for linklet imports
                         import-map   ; sym -> sym-or-import, filled in by `import-name` pass
                         stx-vec stx-mpi
                         portal-stxes))
