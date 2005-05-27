(define-struct key-spec (before after macos unix windows))
(define-struct buff-spec (string start end))

(define global-specs
  (list
   (make-key-spec (make-buff-spec "abc" 1 1)
		  (make-buff-spec "abc" 2 2)
		  (list '(#\f control) '(right))
		  (list '(#\f control) '(right))
		  (list '(#\f control) '(right)))))

(define scheme-specs
  (list 
   (make-key-spec (make-buff-spec "(abc (def))" 4 4)
		  (make-buff-spec "(abc (def))" 10 10)
		  (list ;'(#\f alt control)
			'(right alt))
		  (list ;'(#\f alt control)
			'(right alt))
		  (list ;'(#\f alt control)
			'(right alt)))
   (make-key-spec (make-buff-spec "'(abc (def))" 1 1)
		  (make-buff-spec "'(abc (def))" 12 12)
		  (list ;'(#\f alt control)
			'(right alt))
		  (list ;'(#\f alt control)
			'(right alt))
		  (list ;'(#\f alt control)
			'(right alt)))))
