(module ascii mzscheme

  (provide ascii->char
	   char->ascii
	   ascii-limit
	   ascii-whitespaces)

  (define ascii->char integer->char)
  (define char->ascii char->integer)
  (define ascii-limit 256)
  (define ascii-whitespaces '(32 9 10 11 12 13)))


