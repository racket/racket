#lang scheme/base

(define (write-with-shared-structure val [port (current-output-port)] [optarg #f])
  (parameterize ([print-graph #t]) (write val port)))

(define (read-with-shared-structure [port (current-input-port)] [optarg #f])
  (parameterize ([read-accept-graph #t])
    (read port)))

(provide write-with-shared-structure
         (rename-out [write-with-shared-structure write/ss])
         read-with-shared-structure
         (rename-out [read-with-shared-structure read/ss]))

