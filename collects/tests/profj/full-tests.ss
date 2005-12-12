(module full-tests mzscheme
  (require (lib "profj-testing.ss" "profj")
           (lib "parameters.ss" "profj"))
  
  (prepare-for-tests "Full")
  
  (parameterize ((dynamic? #t))
    (interact-test "class A { }"
                   'full
                   '("dynamic x = new A();" "A a = x;" "(A) a")
                   '((void) (void) a~f)
                   "Casting a guarded/asserted value back to the original type"))
  
  (parameterize ((dynamic? #t))
    (interact-test
     "interface I { int m( int x); }
      class C implements I {
        int m(int x) { return x; }
        boolean n(boolean y) { return !y; }
        dynamic q(I x) { return x; }
      }" 'full
         '("(new C().q(new C())).n(true)" "(new C().q(new C())).m(5)")
         '(error 5)
         "Returning a dynamic value, properly quarded. Should not be send"))
  
  (parameterize ((dynamic? #t))
    (interact-test
     "class X{ int x( int i) { return i; }}"
     'full 
     '("((dynamic) new X()).x(1)" "((dynamic) new X()).x()")
     '(1 error)
     "Test of casting known values to dynamic"))
     
  (execute-test
   "interface A {}
    interface B {}
    class C implements A, B {
    static void go() {
	C c = new C();
	A a = c;
	B b = c;
	
	if (a == b) {
	    b=b;
	}
	if (a == c) {
	    a=a;
	}
	if (c == b) {
	    b=b;
	}
    }
  }" 'full #f "test of ==, using castable")
  
  (execute-test
   "class A { }
    class B extends A { }
    class C extends A { }
    class X {
      A a = new B();
      C b = new C();
      boolean e() {
        return a == b;
      }
    }" 'full #f "Test of ==")
  
  (execute-test
   "class A { }
    class B extends A { }
    class C extends A { }
    class X { 
      B a = new B();
      C b = new C();
      boolean e() {
        return a == b;
      }
    }" 'full #t "Incompatible type test ==")
  
  (execute-test
   "class A {
      boolean b() {
        return \"hi\" == new Object();
      }
    }" 'full #f "Comparing String and Object")
  
  (execute-test
   "final class A {
    }
    interface B { }
    class X {
      Object o( A a ) {
        return (B) a;
      }
    }" 'full #t "Cast from final class to unimpl interface")
  
  (interact-test
   'full
   (list "float x = 3/2;" "x" "double y = 3.2/2;" "y")
   (list '(void) 1 '(void) 1.6) "Test of choosing integer vs floating point division")
  
  (parameterize ((dynamic? #t))
    (execute-test
     "class X { int m(dynamic x) { return x(1); } }"
     'full #f "Using a dynamic parameter as a method"))
  
  (parameterize ((dynamic? #t))
    (execute-test
     "class X { dynamic x; }"
     'full #f "Dynamic variable (unused) in class")
    (execute-test
     "class X { dynamic x; int foo() { return x; } }"
     'full #f "Dynamic variable used, but not executed in class")
    (execute-test
     "class X { dynamic f() { return 3; } }"
     'full #f "Method returning dynamic with actual an int")
    (execute-test
     "class X { int f(dynamic x) { return 3; }}"
     'full #f "Method with dynamic parm, not used")
    (execute-test
     "class X {float f(dynamic x, dynamic y) { return x + y; }}"
     'full #f "Method adding two dynamics, returning a float")
    (interact-test
     "class X { float f( dynamic x, dynamic y) { return x + y; }}"
     'full (list "new X().f(1,1);")
     (list 2) 
     "Method adding two dynamics (returning a float), called"))
    
  
  
  (execute-test
   "class C {
     void x() { return 1; }
    }"
   'full #t "Trying to return value from void method")
  
  (interact-test
   'full
   (list "return 1 + true;")
   (list 'error)
   "Make sure returns are type-checked in interactions")
  
  (execute-test
   "class A {
     void n() { }
     void s() { }
     void src() { }
     void p() { }
     void c() { } }"
   'full #f "Names that used to get clobbered")
  
  (interact-test 
   "class A {
     class B {
       B() { }
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