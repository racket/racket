#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.ss" 40)

(cond
 [(zero? 3) 1111]
 [#f 2222]
 [#t 3333])