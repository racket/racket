(module intermediate-tests mzscheme
  (require "profj-testing.ss")
  
  (prepare-for-tests "Intermediate")
  
  ;;Execute tests without errors
  
  (execute-test
   "interface A { int a(); }
    abstract class B implements A { }
   "
   'intermediate
   #f "abstract class not fully implementing an interface")
  
  (execute-test
   "interface A1 { int a(); }
    abstract class B1 implements A1 { }
    class C1 extends B1 {
     int a() { return 3; }
    }"
   'intermediate
   #f "class implementing abstract class's unimplmenented interface")

  (execute-test
   "interface ToImplement { int a(); }
    abstract class ToExtend implements ToImplement { int a() { return 2; } }
    class ToBe extends ToExtend implements ToImplement {
    }"
   'intermediate
   #f "Repetition of fully satisfied interface in class hierarchy")

  
  (execute-test
   "abstract class Foo {
      abstract int f();
    }"
   'intermediate
   #f "Simple abstract class with abstract method")
  
  (execute-test
   "abstract class Foo1 {
     abstract int f();
    }
    class FooP extends Foo1 {
     int f() { return 3; }
    }"
    'intermediate
    #f "Simple abstract class with extending sub class")
  
  (execute-test
   "abstract class Foo2 {
     abstract int f();
     int fp() { return 3; }
    }
    class FooP2 extends Foo2 {
     int f() { return this.fp(); }
    }"
   'intermediate
   #f "Abstract class with abstract and non abstract methods; implemented")
  
  (execute-test
   "abstract class Fo {
      int dist;
    }"
    'intermediate #f "Abstract class with field")
  
  (execute-test
   "abstract class F {
      abstract int fp();
    }
    abstract class G extends F {
     abstract int gp();
    }"
   'intermediate #f "Abstract class extending abstract class")
  
  (execute-test
   "class first { }
    class second extends first { }"
   'intermediate #f "Class extension")
  
  (execute-test
   "class first1 { int x() { return 3; } }
    class second1 extends first1 { int x() { return 6; }}"
   'intermediate #f "Overriding")
  
  (execute-test
   "class first { int x() { return 3; }}
    class second extends first { int x() { return super.x() + 3; }}"
   'intermediate #f "Use of super in the method body")
  
  (execute-test
   "interface f { int fp(); }
    interface g extends f { int fp(); int gp(); }
    abstract class F implements g {
      int fp() { return 2; }
      abstract boolean i( int c );
    }
    class G extends F {
      int gp() { return 2; }
      boolean i ( int c ) { return c == this.gp(); }
    }"
   'intermediate #f "Abstract class with interface")

  (execute-test 
   "class HasMethod {
     HasMethod() { }
     int myMethod() {
        return 4;
     }
   }
   class HasInheritedMethod extends HasMethod{
    HasInheritedMethod() { }
   
    int otherMethod() {
      return this.myMethod();
    }
   }"
   'intermediate
   #f "Method Inheritance Test")

  (execute-test 
   "class ForError {
  int x;
  }

  class extendForError extends ForError {
  }

  class UseError {
    int f(ForError a) {
    return a.x;
  }
  }

  class NoError extends UseError {
  int fPrime() {
    return this.f( new extendForError() );
  }
  }" 'intermediate #f "Former inheritance error") 

  (execute-test
   "class voidMethod {
      void doNothing() { return; }
   }" 'intermediate #f "Test of return of no arguments")
  
  (execute-test
   "class Date {
     int day;
     int month;
     int year; 

     Date(int day, int month, int year) {
       this.day = day;
       this.month = month;
       this.year = year; 
     }
    }
    class ClockTime {
     int hour;
     int minute;

     ClockTime(int hour, int minute){
       this.hour = hour;
       this.minute = minute;
     }
    }
    abstract class AMuseTicket {
      Date d;
      int price;
    }
    class MuseAdm extends AMuseTicket {
      MuseAdm(Date d, int price) { 
        this.d = d; 
        this.price = price; 
      }
    }
    class OmniMax extends AMuseTicket {
      ClockTime t;
      String title;
      OmniMax(Date d, int price, ClockTime t, String title) { 
        this.d = d; 
        this.price = price; 
        this.t = t;
        this.title = title;
      }
    }
    class LaserShow extends AMuseTicket {
      ClockTime t;
      String row;
      int seat;

      LaserShow(Date d, int price, ClockTime t, String row, int seat) { 
        this.d = d; 
        this.price = price; 
        this.t = t;
        this.row = row;
        this.seat = seat;
      }
    }"
   'intermediate #f "Book Test Date")

  (execute-test
   "abstract class AZooAnimal{
     String name;
     int weight; 
    }
    class Lion extends AZooAnimal{
     int meat;

     Lion(String name, int weight, int meat){
       this.name = name;
       this.weight = weight;
       this.meat = meat;
     }
    }
    class Snake extends AZooAnimal{
     int length;
 
     Snake(String name, int weight, int length){
       this.name = name;
       this.weight = weight;
       this.length = length;
    }
   } 
   class Monkey extends AZooAnimal{
     String food;;

   Monkey(String name, int weight, String food){
     this.name = name;
     this.weight = weight;
     this.food = food;
   }
  }"
  'intermediate #f "Book Test: ZooAnimal")

  (execute-test 
   "class Foo {
           Foo() { 
                  super(); 
                  x = 5;
                       }
               
           int x;
           int adder( int v ) {
                               return v + x;
                                      }
           }"
   'intermediate #f "Calling super")

  (execute-test
   "interface foo {
               int size();
                   }
    class M implements foo {
                         int size() { return 1;}
                                             }"
   'intermediate #f "Interface implemented")

  (execute-test
    "abstract class Path { abstract boolean isOk(); }
     abstract class Success extends Path {
      boolean isOk() { return true; }
     }
     class Left extends Success {
      Success rest;
      Left(Success rest) { this.rest = rest; }
     }"
    'intermediate #f "Abstract method implemented, class subclassed")

  (execute-test
   "class XIof {
     boolean equals( Object o ) {
      return o instanceof XIof;
    }
  }"
   'intermediate #f "Correct instanceof usage")
  
  (execute-test
   "class A { }
    class B extends A { }
    class C {
      A a = new B();
      Object o () { 
        return ((B) a);
      }
      Object ob( B b) {
        return ((A) b);
      }
    }" 'intermediate #f "Simple correct casting")
  
  (execute-test
   "interface A { }
    class C {
      Object e( C c ) {
        return (A) c;
      }
    }" 'intermediate #f "Cast of non-final class to empty interface")
  
  (execute-test
   "interface A { }
    interface B { int foo(); }
    class C {
      Object e( A a ) {
        return (B) a;
      }
    }" 'intermediate #f "Cast of empty interface to non-empty interface")
  
  (execute-test
   "interface A { int foo(); }
    interface B { boolean foo(int i); }
    class C {
      Object e( A a) {
        return (B) a;
      }
    }" 'intermediate #f "Cast of two non-same non-conflicting interfaces")
  
  (execute-test
   "interface A { }
    class C implements A {
       Object e ( C c) {
         return ((A) c);
       }
       Object e2( A a) {
         return ((C) a);
       }
     }" 'intermediate #f "Casts of class to implementing iface, and reverse")
  
  ;;Execute tests with errors
  
  (execute-test
   "
interface I {}

interface J extends I {}

abstract class implements J {}" 'intermediate #t "Parser error, class identifier")
  
  (execute-test
   "class CheckError {
     void foo() { }
   }
   class Examples {
     boolean t1 = check new CheckError().foo() expect false;
   }
   " 'intermediate #t "Check with void method call in test")
  
  (execute-test
   "class CheckError {
    void foo() { }
    }
    class Examples { 
      boolean t1 = check 3 expect new CheckError().foo();
    }" 'intermediate #t "Check with void method call in expect")
  
  (execute-test
   "class A { 
     a b c;
    }"
   'intermediate
   #t "Parse error with three identifiers in a row")
  
  (execute-test
   "interface A { int a(); }
    abstract class B implements A { }
    class C extends B { int a() { return super.a() + 3; } }"
   'intermediate
   #t "Extending class calls super.a() of an abstract method")


  (execute-test
   "interface A { int a(); }
    abstract class B implements A { }
    class C extends B { }"
   'intermediate
   #t
   "Extending class fails to implement abstract parent's unimplemented interfaces")
  
  (execute-test
   "class Foo {
  Foo() {}
  boolean compare(int x, int y, int z) {
    return (x == y) (y == z);
  }
}"
   'intermediate #t "Parse error, two expressions (one parened) without an operator")
  
  (execute-test
   "abstract class F{ abstract int f(); }
    class G extends F { }"
   'intermediate #t "Extending abstract class without method")

  (execute-test
   "abstract class F { abstract int f(); }
    class g extends F { boolean f() { return true; } }"
   'intermediate #t "Extending abstract class with incorrect  method")
  
  (execute-test
   "abstract class F { }
    class G extends F {
      int f() { return super.f(); }
    }"
   'intermediate #t "Super call for non-existing method")
  
  (execute-test
   "class A { int a; }
    class B extends A { int a; }
   " 'intermediate #t "Test of shadowing dissallowed")
  
  (execute-test 
   "interface A { }
    class B extends A { }"
   'intermediate #t "extending an interface")
  
  (execute-test
   "interface A { }
    class B implements A { }
    interface AAA { int f(); }
    abstract class BBBB extends B implements AAA { }
    class NotImplementIFace extends BBBB { }"
   'intermediate #t "Interface not implemented, by inheritance")
   
  (execute-test 
   "abstract class ALoC
  {
  boolean this.isMT;
  abstract boolean compareTo(ALoC that);
  abstract Coach getFst();
  abstract ALoC getRst();
  }"
   'intermediate #t "Illegal use of this as a name")

  (execute-test
   "class ConsList extends List{
            Swimmer first;
            List rest;
            ConsList(Swimmer first, List rest){
            this.first = first;
            this.rest = rest;
     }
            //the longer method takes a number and returns a list of all of the Swimmers
            //in the original list that are longer than the given number.    
               List longer(int n){
                  
                  if (n <= this.first.length) {
                     rest.longer(n);
                  } else { 
                     rest.longer(n)
                     return this.first;
                  }
     }
    }"
   'intermediate #t "Incorrect statement- parse error")
  
  (execute-test
   "interface Filter { Object filt(Object o); }
    class Longer implements Filter{
     double l;
     Longer(double l) {
     this.l=l;
    }
    boolean filt(Object o){
     return (((Swimmer)o).length > l);
    }
   }" 'intermediate #t "Incompatible return type from inherited interface")

  (execute-test
   "class X2 {
      int c (Object o) {
        return (int) o;
      }
    }" 'intermediate #t "Cast of object to primitive")
  
  (execute-test
   "class X3 {
     int c () {
       return (int) false;
     }
   }" 'intermediate #t "cast of boolean to integer")
  
  (execute-test
   "interface A { int x();}
    interface B { boolean x(); }
    class X4 {
      Object o(A a) {
        return (B) a;
      }
    }" 'intermediate #t "cast of incompatible interfaces")
     
  
  ;;Interact tests
  
  (interact-test
   "class cycleA {
     cycleB b;
     cycleA( cycleB b) {
      this.b = b;
      b.addMyA(this);
     }
    }
    class cycleB {
      cycleA a;
      cycleB() { }

      void addMyA( cycleA a) {
        this.a = a;
      }
    }"
   'intermediate
   (list "cycleA a = new cycleA( new cycleB());" "a")
   (list '(void) 'a~f)
   "Cyclic class constructors")
  

  (interact-test
   "class A {
     int a() { return 2; }
    }
    class B extends A {
     int a() { return 3 + super.a(); }
    }"
   'intermediate
   (list "new B().a()")
   (list 5)
   "Calling a super method")

  (interact-test
   'intermediate
   '("(double) 1" "(double) 1.0" "double x;" "x" "x = 1;" "(int) x")
   '(1.0 1.0 (void) 0.0 1.0 1)
   "Double-int conversions")
  
    (interact-test
   "import draw.*;
    import colors.*;
class BlockWorld extends World {
 int WIDTH = 100;
 int HEIGHT = 100;
 AColor BACKGROUND = new Red();
 DrpBlock block;
 BlockWorld(DrpBlock block) {  
  this. block = block;
 }
 World onTick() { 
  return new BlockWorld(this. block.drop());
 }

 World onKeyEvent(String ke) { 
  return this;
 }
 boolean erase() {
  return this. drawBackground();
 }
 boolean draw() {
  return this. block.draw(this); 
 }
 boolean drawBackground() {
  return true;//this. theCanvas.drawRect(new Posn(0,0),this. WIDTH,this. HEIGHT,this. BACKGROUND);
 }
}

class Examples extends BlockWorld { 
 Examples() {
  super(new DrpBlock(10,0));
 }
}
class DrpBlock {
 int down;
 int right;
 int HEIGHT = 10; 
 int WIDTH = 10; 
 int deltaY = 5;
 int deltaX = 3;

 DrpBlock(int down, int right) {
  this. down = down;
  this. right = right;
 }
 DrpBlock drop() {
  return new DrpBlock(this. down + this. deltaY,this. right);
 }
 boolean draw(World w) {
  return w.theCanvas.drawRect(new Posn(this.right,this.down),this.HEIGHT,this.WIDTH,new Red());
 }
 boolean erase(BlockWorld w) {
  return w.theCanvas.drawRect(new Posn(this.right,this.down),this.HEIGHT,this.WIDTH,w.BACKGROUND);
 }
 boolean hasLanded(BlockWorld w) {
  if (this. down + this. HEIGHT >= w.HEIGHT) {
   return true;
  } else {
   return false;
  }
 }
 DrpBlock steer(String ke) {
  if (ke.equals(\"left\")) {
   return new DrpBlock(this. down,this. right - this. deltaX);
  } else {
    if (ke.equals(\"right\")) {
     return new DrpBlock(this. down,this. right + this. deltaX);
    } else { 
      return this; 
    }
   }
 }
 boolean toStop(BlockWorld w, int down) {
  if (this. down + this. HEIGHT >= down) {
   return true;
  } else { 
   return false;
  }
 }
}"
   'intermediate
   '("Examples a = new Examples();") '((void)) 
   "Cycle: used to cause multiple declarations of a class")
  
  (interact-test
   'intermediate
   '("int a = 3;" "a = 45;" "a")
   '((void) 45 45)
   "Test of assignment")
  
  (report-test-results))
