#lang racket/base

(provide unicode-encoding-packages)

(define unicode-encoding-packages
  (string-append
   "\\usepackage[utf8]{inputenc}\n"
   "\\usepackage[T1]{fontenc}\n"))
