
(module dirs mzscheme
  (require (lib "dirs.ss" "setup"))

  (define include-dir (find-include-dir))
  (define std-library-dir (find-lib-dir))

  (provide include-dir std-library-dir))


  
      
