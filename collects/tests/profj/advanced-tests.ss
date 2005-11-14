(module advanced-tests mzscheme
  (require (lib "profj-testing.ss" "profj"))
  
  (prepare-for-tests "Advanced")
  
  (interact-test 'advanced
                 (list "new int[3] instanceof int[] && true"
                       "((int[]) new int[3])[1]" "3/2")
                 (list #t 0 1)
                 "casts and instanceof of array types")
  
  (interact-test 
    "class hasStatic {
     static int x;
     static void setX( int x ) {
      hasStatic.x = x;
     }
    }" 'advanced
    (list "hasStatic.x" "hasStatic.setX(5)" "hasStatic.x")
    (list 0 '(void) 5)
    "Using statics")

  (interact-test
   'advanced
   (list "(new int[2][])[0]" #;"(new int[2][])[1]=new int[2];")
   (list null #;0)
   "multi-dimension array - not all intialized")
  
  (execute-test
   "class TestClass{
     ALoObj iterFilter(){
     for (;false;){}
     }
    }/end SameAuthor"
   'advanced
   #t "Parse error check")
  
  (report-test-results)
  
  )