#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 40)

(cond
 [(zero? 3) 1111]
 [#f 2222]
 [#t 3333])
