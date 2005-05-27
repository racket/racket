;; copyright by Paul Graunke June 2000 AD

(module html-sig mzscheme
  (require (lib "unitsig.ss"))

  (define-signature html-structs^ ((struct html-element (attributes)) (struct html-full (content)) (struct html ()) (struct div ()) (struct center ()) (struct blockquote ()) (struct ins ()) (struct del ()) (struct dd ()) (struct li ()) (struct th ()) (struct td ()) (struct iframe ()) (struct noframes ()) (struct noscript ()) (struct style ()) (struct script ()) (struct basefont ()) (struct br ()) (struct area ()) (struct link ()) (struct img ()) (struct param ()) (struct hr ()) (struct input ()) (struct col ()) (struct isindex ()) (struct base ()) (struct meta ()) (struct option ()) (struct textarea ()) (struct title ()) (struct head ()) (struct tr ()) (struct colgroup ()) (struct thead ()) (struct tfoot ()) (struct tbody ()) (struct tt ()) (struct i ()) (struct b ()) (struct u ()) (struct s ()) (struct strike ()) (struct big ()) (struct small ()) (struct em ()) (struct strong ()) (struct dfn ()) (struct code ()) (struct samp ()) (struct kbd ()) (struct var ()) (struct cite ()) (struct abbr ()) (struct acronym ()) (struct sub ()) (struct sup ()) (struct span ()) (struct bdo ()) (struct font ()) (struct p ()) (struct h1 ()) (struct h2 ()) (struct h3 ()) (struct h4 ()) (struct h5 ()) (struct h6 ()) (struct q ()) (struct dt ()) (struct legend ()) (struct caption ()) (struct table ()) (struct button ()) (struct fieldset ()) (struct optgroup ()) (struct select ()) (struct label ()) (struct form ()) (struct ol ()) (struct ul ()) (struct dir ()) (struct menu ()) (struct dl ()) (struct pre ()) (struct object ()) (struct applet ()) (struct -map ()) (struct a ()) (struct address ()) (struct body ())))

  (define-signature html^ (read-xhtml read-html read-html-as-xml (open html-structs^)
				      use-html-spec))  

  (provide html^))
