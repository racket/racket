(module info (lib "infotab.ss" "setup")
  (define name "SSAX/SXML")
  (define doc.txt "doc.txt")

  (define help-desk-message "Mz/Mr: (require (lib \"ssax.ss\" \"ssax\"))")

  (define blurb
    `("The SSAX/SXML collection provides functions for reading, writing, and manipulating XML documents."))

  (define compile-omit-files
    '("SSAX-code.scm"
      "SXML-to-HTML-ext.scm"
      "SXML-to-HTML.scm"
      "SXML-tree-trans.scm"
      "SXPath-old.scm"
      "define-opt.scm"
      "input-parse.scm"
      "look-for-str.scm"
      "output.scm"
      "ppretty-prints.scm"
      "util.scm"
      "lookup-def.scm")))

 
