#lang scheme/base
(require 2htdp/universe)
(big-bang 1
         (on-tick add1)
         (state #t))