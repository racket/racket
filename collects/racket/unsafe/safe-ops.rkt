#lang racket

(require racket/fixnum
         racket/flonum)


;; the point of this file is to provide functions that are labeled
;; as unsafe but are actually safe.  This provides an easy means to
;; disable unsafety; a require of racket/unsafe/ops can be replaced
;; with a require of racket/unsafe/safe-ops. 

;; this list is almost certainly incomplete; I feel partially justified
;; in adding it to the tree anyhow because 
;; a) it's easy to extend, and
;; b) it appears to me (based on the require of #%unsafe in the corresponding
;;     'ops' library) that determining the full set of functions will require
;;     mucking around in the C source, and not being very confident about 
;;     my conclusions.


(provide (prefix-out unsafe- (all-from-out racket/fixnum))
         (prefix-out unsafe- (all-from-out racket/flonum))
         (prefix-out unsafe- (combine-out vector-length
                                          vector-ref
                                          vector-set!)))