(module full-tests mzscheme
  (require (lib "profj-testing.ss" "profj"))
  
  (prepare-for-tests "Full")
  
  (interact-test 
   "class A {
     class B {
       A m = A.this;
     }
     B b = new B();
    }"
   'full
   (list "A a = new A();" "A.B b = a.new B();" "a.new B().m")
   (list '(void) '(void) 'a~f)
   "Inner class creation")
  
  (execute-test "/* empty */"
                'full
                #f
                "Empty file of comments")
  
  (execute-test 
   "interface Bt { int largestNum(); }

    class Leaf implements Bt { int largestNum() {  return 1 ;} }

    class Heap implements Bt {
     Bt left;	
     Bt right; 	
     int largestNum(){
     if(this.left instanceof Heap &&
        this.right instanceof Heap)
       return this.right.largestNum();
     else if(this.left instanceof Heap)
       return this.right.largestNum();
     else
       return this.right.largestNum();
    }
   }" 'full #f "Instanceof test")

  
  (execute-test "interface F { 
                   int foo(int x);
                 }
                 interface G extends F {
                   int foo(int x);
                 }

                 class A implements G {
                   A() { }
                   int foo(int x) { return 3; }
                 }" 'full #f "Extending an interface while overriding a method")
  
  (execute-test 
   "class Foo {
    private static int getX() { return 5; }
    public static int x = getX();
    }"
   'full #f "Static access and order")

  (interact-test
   "public class hasStatic {
   private int myId;

   public hasStatic( int id ) {
     super();
     this.myId = id;
   }

   public static int returnId( hasStatic s ) {
      return s.myId;
   }
  }"
   'full (list "hasStatic.returnId(new hasStatic(4))") (list 4) "Static use of private field")
  
  (interact-test 'full
                 (list "int x = 4;" "x")
                 (list `(void) 4)
                 "Use of interactions fields")
  
  (interact-test 'full
                 (list "String x = 4;")
                 (list 'error)
                 "Incorrect field assignment")
  
  (interact-test 'full
                 (list "1.0 == 1.0")
                 (list #t)
                 "Floating point ==")
  
  (execute-test
   "class A {
     int x;
     A() {
      this.x = 4;
      super();
     }
    }"
   'full #t "Misplaced super call")
  
  (interact-test
   "class A {
    static int x= 0;
    static {
     for(int i = 0; i< 10; i++)
      x += i;
     }
    }"
   'full (list "A.x") (list 45)  "for loop in static section")
  
  (execute-test
   "class A { A() { super.toString(); } }"
   'full #f "Calling a super method")
  
  (report-test-results))