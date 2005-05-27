(module why mzscheme
  (require "../private/headelts.ss"
           "../private/util.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    
    (report-errors-to-browser send/finish)
    
    `(HTML 
      (HEAD ,hd-css
            ,@hd-links
            (TITLE "Why DrScheme?"))
      (BODY 
       (H1 "Why DrScheme?") 
       "Teaching introductory computing courses with Scheme, or any other "
       "functional programming language, facilitates many conceptual tasks "
       "and greatly enhances the appeal of computer science. Specifically, "
       "students can implement many interesting programs with just a "
       "small subset of the language. The execution of a functional program "
       "can be explained with simple reduction rules that students mostly "
       "know from secondary school.  Interactive implementations allow "
       "for quick feedback to the programmers andmake the development of "
       "small functions a pleasant experience." 
       (P)
       "Unfortunately, the poor quality of the available environments "
       "for functional languages negates these advantages. Typical "
       "implementations accept too many definitions, that is, definitions "
       "that are syntactically well-formed in the sense of the full "
       "language but meaningless for beginners. The results are "
       "inexplicable behavior, incomprehensible run-time errors, or "
       "confusing type error messages. The imperative nature of "
       "read-eval-print loops often introduces subtle bugs into "
       "otherwise perfect program developments. Scheme, in particular, "
       "suffers from an adherence to Lisp's output traditions, which "
       "often produces confusing effects. In many cases students, "
       "especially those familiar with commercial C++ environments, "
       "mistake these problems for problems with the functional "
       "approach and reject the approach itself."
       (P)
       "To overcome this obstacle, we have developed a new programming "
       "environment for Scheme. It fully integrates a (graphics-enriched) "
       "editor, a multi-lingual parser that can process a hierarchy "
       "of syntactically restrictive variants of Scheme, a functional "
       "read-eval-print loop, and an algebraically sensible printer. "
       "The environment catches the typical syntactic mistakes of "
       "beginners and pinpoints the exact source location of run-time "
       "exceptions. The new programming environment also provides "
       "an algebraic stepper and a static debugger. The former reduces "
       "Scheme programs, including programs with assignment and "
       "control effects, to values (and effects). The static debugger "
       "infers what set of values an expression may produce and how "
       "values flow from expressions into variables. It exposes potential "
       "safety violations and, upon demand from the programmer, explains "
       "its reasoning by drawing value flowgraphs over the program text. "
       "Preliminary experience with the environment shows that "
       "students find it helpful and that they greatly prefer it to "
       "shell-based or Emacs-based systems."
       (P)
       "A paper that discusses DrScheme in "
       "more detail is available in the paper: " 
       (A ((HREF "http://www.ccs.neu.edu/scheme/pubs#jfp01-fcffksf")
           (TARGET "_top")) "DrScheme: A Programming Environment for Scheme") "."))))