(module frtime-big "frtime.ss"
  
  (require (frlibs "list.ss" "etc.ss" "math.ss" "date.ss"))
  
  (provide (all-from "frtime.ss")
           (all-from (lib "list.ss" "frtime"))
           (all-from (lib "etc.ss" "frtime"))
           (all-from (lib "math.ss" "frtime"))
           (all-from (lib "date.ss" "frtime"))))
