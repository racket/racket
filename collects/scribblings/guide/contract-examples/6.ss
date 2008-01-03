;; chapter 6: Subclassing and Contracts

;; In a subclass, an Eiffel programmer must use
;;    ... require else ...
;; for preconditions, which or-s them together, and
;;    ... ensure then ...
;; for postconditions, which and-s them together.

;; See Findler's papers on inheritance and contracts for
;; an analysis of these mechanisms and their problems. 

;; prepare for subclassing with postconditions of the shape:

;;  precondition1 <b>implies</b> postcondition1
;;  and 
;;  ... 
;;  and 
;;  preconditionN <b>implies</b> postconditionN

;; [Note: I am not sure about this one yet.]
