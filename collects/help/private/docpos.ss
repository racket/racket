(module docpos mzscheme
  (require (lib "list.ss")
           (lib "contract.ss"))

  ;; Define an order on the standard docs.
  (define (standard-html-doc-position d)
    (let ([str (path->string d)])
      (if (equal? str "help")
          -1
          (let ([line (assoc str docs-and-positions)])
            (if line
                (caddr line)
                100)))))


  ;; (listof (list string string number))
  ;; the first string is the collection name
  ;; the second string is the title of the the manual
  ;; the number determines the sorting order for the manuals in the manuals page
  (define docs-and-positions
    `(("r5rs" "Revised^5 Report on the Algorithmic Language Scheme" -50)
      ("mzscheme" "PLT MzScheme: Language Manual" -49)
      ("mred" "PLT MrEd: Graphical Toolbox Manual" -48)

      ("tour" "A Brief Tour of DrScheme" 0)
      ("drscheme" "PLT DrScheme: Programming Environment Manual" 1)

      ("srfi" "SRFI documents inside PLT" 3)

      ("mzlib" "PLT MzLib: Libraries Manual" 5)
      ("misclib" "PLT Miscellaneous Libraries: Reference Manual" 6)
      ("mrlib" "PLT MrLib: Graphical Libraries Manual" 7)
      ("framework" "PLT Framework: GUI Application Framework" 8)

      ("mzc" "PLT mzc: MzScheme Compiler Manual" 10)
      ("foreign" "PLT Foreign Interface Manual" 10)

      ("tools" "PLT Tools: DrScheme Extension Manual" 30)
      ("insidemz" "Inside PLT MzScheme" 50)

      ("web-server" "Web Server Manual" 60)
      ("swindle"    "Swindle Manual"    61)
      ("plot"       "PLoT Manual"       62)

      ("t-y-scheme" "Teach Yourself Scheme in Fixnum Days" 100)
      ("tex2page" "TeX2page" 101)

      ("beginning" "Beginning Student Language" 200)
      ("beginning-abbr" "Beginning Student with List Abbreviations Language" 201)
      ("intermediate" "Intermediate Student Language" 202)
      ("intermediate-lambda" "Intermediate Student with Lambda Language" 203)
      ("advanced" "Advanced Student Language" 204)
      ("teachpack" "Teachpacks for How to Design Programs" 205)
      ("teachpack-htdc" "Teachpacks for How to Design Classes" 206)

      ("profj-beginner" "ProfessorJ Beginner Language" 210)
      ("profj-intermediate" "ProfessorJ Intermediate Language" 211)
      ("profj-advanced" "ProfessorJ Advanced Language" 212)))

  (define known-docs (map (lambda (x) (cons (string->path (car x)) (cadr x))) docs-and-positions))
  
  (provide/contract
   [standard-html-doc-position (path? . -> . number?)]
   [known-docs (listof (cons/c path? string?))]))
