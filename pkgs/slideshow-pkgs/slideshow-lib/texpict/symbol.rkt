(module symbol slideshow/slideshow
  
  (provide symbol
	   sym:in sym:rightarrow sym:leftarrow sym:infinity sym:times 
	   sym:implies sym:emdash 
	   sym:therefore)
  
  (define (symbol n)
    (text (string (integer->char n)) 'symbol font-size))

  (define sym:in (symbol #x2208))
  (define sym:rightarrow (symbol #x2192))
  (define sym:leftarrow (symbol #x2190))
  (define sym:infinity (symbol #x221E))
  (define sym:times (symbol 215))
  (define sym:implies (symbol #x21D2))
  (define sym:emdash (symbol #x2014))
  (define sym:therefore (symbol #x2234)))


