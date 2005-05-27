(module char-encodings mzscheme

  (require "ascii.ss")

  (provide ucscode->char
	   char-return char-tab char-newline)

  (define ucscode->char ascii->char)
  (define char-return (ascii->char 13))
  (define char-tab (ascii->char 9))
  (define char-newline (ascii->char 10)))
