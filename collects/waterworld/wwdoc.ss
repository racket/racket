(module wwdoc mzscheme
  (require (lib "xml.ss" "xml"))

  (define *the-file* "ww.html")
  (define *the-page* 
    `(html 
      (body 
       "You are exploring WaterWorld, where each location is either "
       "empty sea, or contains pirates. "
       "When you enter a location, you must "
       (em "correctly") " "
       "anticipate whether or not it contains pirates:"
       (ul 
	(li 
	 "If you anticipate pirates, shift-click on the location, "
	 "meaning you've brought gold "
	 "to appease them with."
	 (ul 
	  (li 
	   "If you are correct, the pirates relieve you of your burden "
	   "before it pulls you down, and let you live.")
	  (li 
	   "However, if you are incorrect and there were no pirates, "
	   "then the gold "
	   "weighs you down, and you drown.")))
	(li 
	 "If you anticipate the location is empty, just click on the "
	 "location, meaning you "
	 (em  "haven't") " "
	 "brought gold"
	 (ul 
	  (li 
	   "If you are correct, you can measure the "
	   "pollution content of the water, "
	   "and find out how many neighboring locations have pirates "
	   "(though not which ones).")
	  (li 
	   "However, if you are incorrect and there are pirates, "
	   "they will keel-haul you, then impress you into slavery."))))
       (p)
       "Note the asymmetry of the situation: you get further information "
       "only after exploring empty locations."
       (hr)
       (h2 "Format of games")
       "WaterWorld games are stored as S-expressions in the following format:"
       (pre 
	"(game" (br)
	" (rows n)" (br)
	" (columns m)" (br) 
	" (locations" (br)
	"  (location (row p) (column q) (safe? b) (concealed? c))" (br)
	"   ..." (br)
	" ))")
       "where " 
       (tt "n" ) ", "
       (tt "m" ) ", "
       (tt "p" ) ", and "
       (tt "q" ) " are numbers, and "
       (tt "b" ) " and "
       (tt "c" ) " are Scheme booleans. "
       "The locations must be enumerated in row-major order.  That is, "
       "all columns within a row are given, in order, before listing the "
       "next row.  Both rows and columns are enumerated from 0.")))

  (when (file-exists? *the-file*)
	(delete-file *the-file*))
  (let ([html-port (open-output-file *the-file*)])
    (write-xml/content
     (xexpr->xml *the-page*) html-port)
    (close-output-port html-port)))
