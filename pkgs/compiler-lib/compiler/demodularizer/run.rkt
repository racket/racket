#lang racket/base

(provide (struct-out run))

(struct run (path/submod phase linkl meta-linkl uses
                         stx-vec stx-mpi
                         portal-stxes))
