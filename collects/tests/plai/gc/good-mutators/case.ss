#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.ss" 40)
(test/value=? (case 1 [(1) 2])
              2)
(test/value=? (case 1 [(1) 2] [else 3])
              2)
(test/value=? (case 2 [(1) 2] [else 3])
              3)