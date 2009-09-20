(module full-tests mzscheme
  (require "profj-testing.ss"
           (lib "parameters.ss" "profj"))
  
  (prepare-for-tests "Full")

  (execute-test
   "class AnExceptionThrower {
      int m() throws Throwable {
        if (true)
          throw new Throwable();
        throw new Exception();
      }
    }" 'full #f "Throwable is a subclass of Throwable for purpose of throws clause")
  
  (execute-test
   "class AnotherExceptionThrower {
     int m() throws Exception {
        throw new Exception();
    }}" 'full #f "Throwable is imported when using throw")
  
  (interact-test
   "class YAET {
      int m() throws Exception {
        throw new Exception();
      }
    }"
   'full '("check new YAET().m() catch Exception" "check new YAET().m() catch Throwable")
   '(#t #t) "Check properly catching exceptions")
  
  (execute-test
   "import java.util.*;
    class Random { }"
   'full #f "Hiding an import * name with a local class"
   )
  
  (interact-test
   "import java.util.*;
    class Random {
      int getInt() { return 3; }
   }"
   'full '("new Random().getInt()") '(3)
   "Using the local Random and not the imported one")
  
  (interact-test
   "class allPublic {
      public int x() { return 3; }
    }
    class onePrivate {
      private int x() { return new allPublic().x(); }
      public int y() { return x(); }
    }
    "
   'full
   '("new onePrivate().y()") '(3) "Private method calling public method of same name")
  
  (execute-test
   "class withPrivate {
      withPublic f;

      private int with() { return this.f.with(); }
    }

    class withPublic {
      withPrivate r = new withPrivate();

      public int with() { return 3; }
    }" 'full #f "Potential conflict of names for private method")
  
  (execute-test 
   "class hasCharArray {  
     char[] b = new char[]{'a'};
   }" 'full #f "Test of array alloc init")
  
  (execute-test
   "class Aextendee {
     int f (Aextendee x) { return 4; }
    }
    class Bextendor extends Aextendee {
     int f( Bextendor x) { return 5; }
    }"
   'full #f 
   "Overloading introduced on extends")
  
  (execute-test
   "class Xforward {
     int x = y;
     int y;
   }" 'full #t "Forward reference error")
  
  (interact-test 
   "class Xnoundef {
     int x = this.y;
     int y = 2;
    }"
   'full
   '("new Xnoundef().x" "new Xnoundef().y")
   '(0 2)
   "Testing no undefined fields")
  
  (parameterize ((dynamic? #t))
    (interact-test
     "class Xeq { }"
    'full
    '("Xeq x = new Xeq();" "Xeq y = (dynamic) x;" "x.equals(y)" "y.equals(x)" "y==x" "x==y")
    '((void) (void) #t #t #t #t)
    "Equality test of a wrapped and unwrapped object"))
  
  (parameterize ((dynamic? #t))
    (interact-test
    "class Xacc { int y; Xacc(int y) { this.y = y; } }"
    'full 
    '("Xacc x = new Xacc(3);" "Xacc y = (dynamic) x;"  "x.y = 4"  "y.y" "y.y=5" "x.y")
    '((void) (void) 4 4 5 5)
    "Accessing fields of a dynamic value"))
  
  (execute-test
   "package a; class a { int x; 
                     Object get() { class b { int y() { return a.this.x; } }  return new b(); }}"
   'full #f "Statement inner class accessing package field")
  
  (parameterize ((dynamic? #t))
    (interact-test "class Acast { }"
                   'full
                   '("dynamic x = new Acast();" "Acast a = x;" "(Acast) a")
                   '((void) (void) a~f)
                   "Casting a guarded/asserted value back to the original type"))
  
  (parameterize ((dynamic? #t))
    (interact-test
     "interface I { int m( int x); }
      class C implements I {
        public int m(int x) { return x; }
        boolean n(boolean y) { return !y; }
        dynamic q(I x) { return x; }
      }" 'full
         '("(new C().q(new C())).n(true)" "(new C().q(new C())).m(5)")
         '(error 5)
         "Returning a dynamic value, properly quarded. Should not be send"))
  
  (parameterize ((dynamic? #t))
    (interact-test
     "class Xcastd{ int x( int i) { return i; }}"
     'full 
     '("((dynamic) new Xcastd()).x(1)" "((dynamic) new Xcastd()).x()")
     '(1 error)
     "Test of casting known values to dynamic"))
     
  (execute-test
   "interface Aa {}
    interface Ba {}
    class Ca implements Aa, Ba {
    static void go() {
	Ca c = new Ca();
	Aa a = c;
	Ba b = c;
	
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
   "class Ab { }
    class Bb extends Ab { }
    class Cb extends Ab { }
    class Xb {
      Ab a = new Bb();
      Cb b = new Cb();
      boolean e() {
        return a == b;
      }
    }" 'full #f "Test of ==")
  
  (execute-test
   "class Ac { }
    class Bc extends Ac { }
    class Cc extends Ac { }
    class Xc { 
      Bc a = new Bc();
      Cc b = new Cc();
      boolean e() {
        return a == b;
      }
    }" 'full #t "Incompatible type test ==")
  
  (execute-test
   "class Ad {
      boolean b() {
        return \"hi\" == new Object();
      }
    }" 'full #f "Comparing String and Object")
  
  (execute-test
   "final class Ae {
    }
    interface Be { }
    class Xe {
      Object o( Ae a ) {
        return (Be) a;
      }
    }" 'full #t "Cast from final class to unimpl interface")
  
  (interact-test
   'full
   (list "float x = 3/2;" "x" "double y = 3.2/2;" "y")
   (list '(void) 1 '(void) 1.6) "Test of choosing integer vs floating point division")
  
  (parameterize ((dynamic? #t))
    (execute-test
     "class Xf { int m(dynamic x) { return x(1); } }"
     'full #f "Using a dynamic parameter as a method"))
  
  (parameterize ((dynamic? #t))
    (execute-test
     "class Xg { dynamic x; }"
     'full #f "Dynamic variable (unused) in class")
    (execute-test
     "class Xh { dynamic x; int foo() { return x; } }"
     'full #f "Dynamic variable used, but not executed in class")
    (execute-test
     "class Xi { dynamic f() { return 3; } }"
     'full #f "Method returning dynamic with actual an int")
    (execute-test
     "class Xj { int f(dynamic x) { return 3; }}"
     'full #f "Method with dynamic parm, not used")
    (execute-test
     "class Xk {float f(dynamic x, dynamic y) { return x + y; }}"
     'full #f "Method adding two dynamics, returning a float")
    (interact-test
     "class Xl { float f( dynamic x, dynamic y) { return x + y; }}"
     'full (list "new Xl().f(1,1);")
     (list 2) 
     "Method adding two dynamics (returning a float), called"))
    
  
  
  (execute-test
   "class Crv {
     void x() { return 1; }
    }"
   'full #t "Trying to return value from void method")
  
  (interact-test
   'full
   (list "return 1 + true;")
   (list 'error)
   "Make sure returns are type-checked in interactions")
  
  (execute-test
   "class Abames {
     void n() { }
     void s() { }
     void src() { }
     void p() { }
     void c() { } }"
   'full #f "Names that used to get clobbered")
  
  (interact-test 
   "class Ainner {
     class B {
       B() { }
       Ainner m = Ainner.this;
     }
     //B b = new B();
    }"
   'full
   (list "Ainner a = new Ainner();" "Ainner.B b = a.new B();" "a.new B().m")
   (list '(void) '(void) 'a~f)
   "Inner class creation")
  
  (execute-test "/* empty */"
                'full
                #f
                "Empty file of comments")
  
  (execute-test 
   "interface Bt { int largestNum(); }

    class Leaf implements Bt { public int largestNum() {  return 1 ;} }

    class Heap implements Bt {
     Bt left;	
     Bt right; 	
     public int largestNum(){
     if(this.left instanceof Heap &&
        this.right instanceof Heap)
       return this.right.largestNum();
     else if(this.left instanceof Heap)
       return this.right.largestNum();
     else
       return this.right.largestNum();
    }
   }" 'full #f "Instanceof test")

  
  (execute-test "interface Faa { 
                   int foo(int x);
                 }
                 interface Gaa extends Faa {
                   int foo(int x);
                 }

                 class Aia implements Gaa {
                   Aia() { }
                   public int foo(int x) { return 3; }
                 }" 'full #f "Extending an interface while overriding a method")
  
  (execute-test 
   "class Foo {
    private static int getX() { return 5; }
    public static int x = getX();
    }"
   'full #f "Static access and order")

  (interact-test
   "public class hasStatic1 {
   private int myId;

   public hasStatic1( int id ) {
     super();
     this.myId = id;
   }

   public static int returnId( hasStatic1 s ) {
      return s.myId;
   }
  }"
   'full (list "hasStatic1.returnId(new hasStatic1(4))") (list 4) "Static use of private field")
  
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
   "class Az {
    static int x= 0;
    static {
     for(int i = 0; i< 10; i++)
      x += i;
     }
    }"
   'full (list "Az.x") (list 45)  "for loop in static section")
  
  (execute-test
   "class Azz { Azz() { super.toString(); } }"
   'full #f "Calling a super method")
  
  (report-test-results))
