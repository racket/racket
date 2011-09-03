#lang typed/racket

(ann (for ([#{i : Integer} '(1 2 3)]) (display i)) Void)
