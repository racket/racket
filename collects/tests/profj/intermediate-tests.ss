(module intermediate-tests mzscheme
  (require (lib "profj-testing.ss" "profj"))
  
  (prepare-for-tests "Intermediate")
  
  ;;Execute tests without errors

  (execute-test
   "abstract class Foo {
      abstract int f();
    }"
   'intermediate
   #f "Simple abstract class with abstract method")
  
  (execute-test
   "abstract class Foo {
     abstract int f();
    }
    class FooP extends Foo {
     int f() { return 3; }
    }"
    'intermediate
    #f "Simple abstract class with extending sub class")
  
  (execute-test
   "abstract class Foo {
     abstract int f();
     int fp() { return 3; }
    }
    class FooP extends Foo {
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
   "class first { int x() { return 3; } }
    class second extends first { int x() { return 6; }}"
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
   "class X {
     boolean equals( Object o ) {
      return o instanceof X;
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
                  
                  if (n <= this.first.length)
                     rest.longer(n);
                  else 
                     rest.longer(n)
                     return this.first;
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
   "class X {
      int c (Object o) {
        return (int) o;
      }
    }" 'intermediate #t "Cast of object to primitive")
  
  (execute-test
   "class X {
     int c () {
       return (int) false;
     }
   }" 'intermediate #t "cast of boolean to integer")
  
  (execute-test
   "interface A { int x();}
    interface B { boolean x(); }
    class X {
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


  
  (report-test-results))