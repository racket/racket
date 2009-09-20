(module beginner-tests mzscheme
  (require "profj-testing.ss")
  (require mzlib/class
           profj/libs/java/lang/Object
           profj/libs/java/lang/String)
  
  (prepare-for-tests "Beginner")
  
  (define language 'beginner)

  ;;Execution tests that should pass
  
  (execute-test
   "class MyClass {
      Object field;

      MyClass( Object f ) { this.field = f; }      

      MyClass method() { return this; }
    }

    class CorrectChecks {
     
     boolean t;
     boolean t2 = check 1 expect 3;
     boolean t3 = (check 1 expect 3) || (check 2 expect 3);
     boolean t4 = (check 1 expect 1) && (check 3 expect 3);
     boolean t5 = check \"Hi\" expect \"\";
     boolean t6 = check 1.5 expect 2 within .4;
     boolean t7 = check true expect false;
     boolean t8 = check new MyClass(\"\") expect new MyClass(\"\");
     boolean t9 = check new MyClass(\"\").field expect \"\";
     boolean t10 = check new MyClass(\"\").method() expect new MyClass(\"\");

     CorrectChecks(boolean t) { this.t= t; }

   }" language #f "Class with many different style of checks within it")
  
  (execute-test 
   "interface A {
      boolean s( B b);
    }
    class B implements A {
      B() { }
      boolean s( B b ) { return true; }
    }"
   language #f "Interface and class with cyclic reference")               
  
  (execute-test
   "class Simple {
     Simple() { }
    }
   class Simple2 {
     Simple s;
     Simple2( Simple s ) {
       this.s = s;
     }
   }
   class Simple3 {
    Simple2 s;
    Simple3( Simple2 s ) {
      this.s = s;
    }
    boolean happy() {
      return true;
    }
   }"
   language 
   #f
   "Simple classes, with fields and methods")

  (execute-test
   "interface F {
     int fo();
    }"
   'beginner #f "Interface with method")
  
  (execute-test
   "interface F {
     int foo();
    }
    class FP implements F {
      int x;
      FP(int x) {
        this.x = x;
      }
      int foo() { return this.x; }
    }"
   language
   #f
   "Simple implementation of interface")
  
  (execute-test
   "interface F {
      int lessThan( int x );
    }
    class Fp implements F {
      int x;
      Fp( int x ) {
        this.x = x;
      }
      int lessThan( int y) {
         if (y < this.x) {
           return -1;
         } else {
           return 1;
         }
      }
    }" language #f "Class & interface, containing if statement")
  
  (execute-test
   "class A {
     int a;
     double b;
     boolean c;
     char d;
     String e;
     Object f;

     A(int a, double b, boolean c, char d, String e, Object f) {
       this.a = a;
       this.b = b; this.c =c; this.d = d; this.e = e; this.f = f;
     }
   }" language #f "Class with variables of many primitive types")
  
  (execute-test
    "class A {
      B var;
      A(B var) { this.var = var; }
     }
     class B {
      A var;
      B( A var) { this.var = var; }
     }" language #f "Two classes with cycles: cannot be instantiated")
  
  (execute-test
   "interface X { }
    class O {
     O() { }
     String happy( X x ) {
       return x.toString();
     }
   }" language #f "Test that interface types have Object methods")
  
  ;;Execution tests that should produce errors

  (execute-test
   "class UseNoSet {
       Object x;
       Object y = this.x;
       UseNoSet(Object x) { this.x = x; }
    }" language #t "using fields before setting them")
  
  (execute-test
   "class DoubleSet {
     int x;
     DoubleSet(int x, int y) {
       this.x = x;
       this.x = y;
     }
   }" language #t "Setting a field twice, in the constructor")
  
  (execute-test
   "class DoubleSet2 {
     int x = 3;
     DoubleSet2(int x) {
       this.x = x;
     }
   }" language #t "Setting a field twice, init and ctor")
  
  (execute-test
   "class CorrectChecks {
     
     boolean t;
     boolean t2 = check 1 expect 3;
     boolean t3 = (check 1 expect 3) || (check 2 expect 3);
     boolean t4 = (check 1 expect 1) && (check 3 expect 3);
     boolean t5 = check \"Hi\" expect \"\";
     boolean t6 = check 1.5 expect 2 within .4;
     boolean t7 = check true expect false;
     boolean t8 = check new MyClass(\"\") expect new MyClass(\"\");
     boolean t9 = check new MyClass(\"\").field expect \"\";
     boolean t10 = check new MyClass(\"\").method() expect new MyClass(\"\");

     ()
     
     CorrectChecks(boolean t) { this.t= t; }

   }" language #t "Correct checks, followed by a parse error: should mention (")
  
  (execute-test
   "class X {
     int x = this.y;
     int y = 3;
     X() { }
   }" language #t "Should be forward field error")
  
  (execute-test
   "interface Z { 
     int x(); 
    }

   class A {
    int z = 3;
    Z y;
    A(int z) { 
     this.z = z;
     this.y = y;//new B();
    } 
   }

   class B implements Z {
    B() { }
    int x() { return 3; }
    int oX() { if (this.x() == 3) { return 5; } else { return 6; } }
   }
  foo"
   language #t "Parse-error test, mentioning foo")

  (execute-test
   "class X { 
      X() { super(); }
    }"
   language #t "Parse-error test, indicating that super not allowed")

  (execute-test
   "class X {
     X() { }
     int x() { return super.x(); }
    }"
   language #t "Parse-error test, indicating that super.x() not allowed")
  
  (execute-test
   "interface A {
      int x;
    }"
   language #t "Interface with a field")
  
  (execute-test
   "interface A {
      int x();
      boolean x();
    }" language
       #t "Two methods in an interface with the same name, and different return types")

  (execute-test
   "interface A {
      int x();
      int x(int y);
    }" language
       #t "Two methods in an interface with different arguments")

  (execute-test
   "public class B { B() { } }"
   language #t "Use of public")
  
  (execute-test
   "interface A {
     A foo();
    }
   class B implements A {
     B() { }
     A foo( String s) { return new B(); }
   }"
   'beginner #t "Method not implemented test, overload error")
  
  (execute-test
   "class F { }"
   'beginner #t "No constructor error")
  
  (execute-test
   "class F {
      F() { }
      int x;
      int x() { return 3; }
   }" 'beginner #t "Method and field name share names error")
     
  (execute-test 
   "class badCtor {               
     badCtor() c{
     } 
    }" 'beginner #t "Error message bug")

  
  (execute-test
   "class IncorrectField {
     int myField;
     IncorrectField(int x) {
       this.myField = myField;
     }
   }"
   'beginner #t "Field access without this")
  
  (execute-test
   "class IncorrectMethodCall {
    IncorrectMethodCall() { }
    int returner() { return 4; }
    int adder( int x ) {
      return returner() + x; 
    }
   }"
   'beginner #t "Method access without this")
  
  (execute-test
   "class F1 {
     F1(int x) {
      x = x;
     }
    }"
   'beginner #t "Set non-field")
  
  (execute-test
    "class F2 {
      int f;
      F2() {
       this.f = f;
      }
     }"
    'beginner #t "Set with field")
  
  (execute-test
   "interface A {
     A foo();
    }
    class B implements A {
      B() { }
      B foo() { return new B(); }
    }"
   'beginner #t "Incorrect overriding")
    
  (execute-test
   "abstract class A { }"
   language #t "Use of abstract class")
  
  (execute-test
   "class B extends A { }"
   language #t "Use of extends")
  
  (execute-test
   "class A { A() { } }
    class B implements A { B () { } }"
   language #t "Implementing a class")
  
  (execute-test
   "import java.util.Random;
    class Random { }"
   language #t "Renaming an imported class")
  
  (execute-test
   "import geometry.*;
    class Posn { }"
   language #t "Renaming an imported class with a star")
  
  ;;Interaction tests: Mix of pass and fail
  
  (interact-test
   language
   '("1 + 2 * 3 / 4" "new Object()" "\"Hello World\"" "true" "true && false" "true || false")
   (list 2 (make-object Object) (make-java-string "Hello World") #t #f #t)
   "Tests of simple expressions")
  
  (interact-test
   "class Book {
      String title;
      int numPages;
      Book( String title, int numPages ) {
        this.title = title;
        this.numPages = numPages;
      }
    }"
   language
   (list "Book b = new Book(\"HP\", 33);" "b.title" "new Book(\"HP\",33).numPages" "new Book()" "new Book(\"\",0).numPages()")
   (list (void) (make-java-string "HP") 33 'error 'error)
   "Tests instantiating a class")
  
  (interact-test
   "class Book2 {
     int numPages;
     Book2( int numPages ) {
       this.numPages = numPages;
     }
     String level() {
       if ( this.numPages < 10 ) {
         return \"Apprentice\";
       } else { 
         if (this.numPages < 100) {
           return \"Journeyman\";
         } else {
           return \"Master\";
         } 
       }
     }
   }
   "
   language
   (list "new Book2(9).level()" "new Book2(10).level()" "new Book2(100).level()" "new Book2(99).level")
   (list (make-java-string "Apprentice") (make-java-string "Journeyman") (make-java-string "Master") 'error)
   "Tests of an if in a method")
  
  (interact-test
   "interface AShape { }
    class CartPt {
      int x; 
      int y; 

      CartPt(int x, int y) {
        this.x = x; 
        this.y = y; 
      }
    }
    class Dot implements AShape {
      CartPt loc;
      Dot(CartPt loc) { 
        this.loc = loc; 
      }
    }
    class Square implements AShape {
      CartPt loc;
      int size; 

      Square(CartPt loc, int size) {
        this.loc = loc; 
        this.size = size; 
      }
    }
    class Circle implements AShape {
      CartPt loc;
      int radius; 

      Circle(CartPt loc, int radius) {
        this.loc = loc; 
        this.radius = radius; 
      }
    }"
   language
   (list "new Object()" "new AShape()" "new Square(new CartPt(77,22), 300).size" "new CartPt(21,1+21)")
   (list `(make-object Object) 'error 300 `(let ((obj (make-object CartPt)))
                                             (send obj CartPt-constructor-int-int 21 22)
                                             obj))
   "Book Test: AShape")
  
  (interact-test 
   "class Coffee {
     String kind;
     int price;
     int weight;

     Coffee(String kind, int price, int weight) {
       this.kind = kind;
       this.price = price; 
       this.weight = weight; 
     }
 
     // to compute the total cost of this coffee purchase
     int cost() { return this.price * this.weight; }
 
    // to determine whether this coffee costs less than the given amount
    boolean costsLess(int amt) { return this.price < amt; }
 
    // to determine whether this coffee is cheaper than the given coffee
    boolean cheaperThan(Coffee that) { return this.price <  that.price; }
 
  }"
   'beginner
   (list "new Coffee(\"Kona\", 1595, 100).cost()"
         "new Coffee(\"Ethiopian\", 800, 1000).cost()"
         "new Coffee(\"Colombian\", 950, 20).cost()"
         "new Coffee(\"Kona\", 1595, 100).costsLess(200)"
         "new Coffee(\"Columbian\", 950, 200).costsLess(1000)"
         "new Coffee(\"Ethiopian\", 800, 1000).cheaperThan(new Coffee(\"Columbian\", 950, 200))"
         "new Coffee(\"Kona\", 1595, 100).cheaperThan(new Coffee(\"Columbian\", 950, 200))")
   (list 159500 800000 19000 #f #t #t #f)
   "Book Test Coffee")
  
  (interact-test 'beginner
                 (list "Math.abs(-1)")
                 (list 1)
                 "Library availability test")
                
  (execute-test
   "// Exercise 3.1.1
    // Exercise 5.1.2
    interface ALoH{}
    class MTLoH implements ALoH{
	MTLoH(){}
    }
    class ConsHouse implements ALoH{
	House fst;
	ALoH rst;

	ConsHouse(House f, ALoH r) {
		this.fst = f;
		this.rst = r;
	}
    }
    class Address {
      int no;
      String street;
      String city;

      Address(int no, String street, String city) {
        this.no = no;
        this.street = street;
        this.city = city;
      }
    }
   class House {
     String kind;
     int rooms;
     Address address;
     int price;

     House(String kind, int rooms, Address address, int price) {

       this.kind = kind;
       this.rooms = rooms;
       this.address = address;
       this.price = price;
     }
 
     // determine if this house has more rooms (is bigger) than that given house
     boolean isBigger(House that) { return this.rooms > that.rooms; }
     // actual:  h1.isBigger(h2), expected:  false
     // actual:  h2.isBigger(h3), expected:  true

     //determine if this house's city as the same as a given city
     boolean thisCity(String city) { return this.city.equals(city); }

     // h1.thisCity(\"Brookline)\"--true
     // h2.thisCity(\"Brookline\")--false
   }

   /*
   Address a1 = new Address(23, \"Maple Street\", \"Brookline\");
   Address a2 = new Address(5, \"Joye Road\", \"Newton\");
   Address a3 = new Address(83, \"Winslow Street\", \"Waltham\");

   House h1 = new House(\"Ranch\", 7, a1, 375000);
   House h2 = new House(\"Colonial\", 9, a2, 450000);
   House h3 = new House(\"Cape\", 6, a3, 235000);

   ALoH mtlist = new MTLoH();
   ALoH list1 = new ConsHouse(h1, mtlist);
   ALoH list2 = new ConsHouse(h3, list1);
   */"
   'beginner #t "Error message bug")
  
  (interact-test
   "interface A { }"
   language
   (list "new A()")
   (list 'error)
   "Trying to create an instance of an interface")
  
  (interact-test
   "class X { X() { } double f() { return 2; } }"
   language
   (list "double x = 1;" "x" "new X().f()")
   (list '(void) 1.0 2.0)
   "Converting ints into doubles appropriately")
  
  (interact-test
   language
   (list "check true expect true"
         "check true expect 1"
         "check true expect true within 1"
         "check new Object() expect \"hi\""
         "check \"hi\" expect new Object()"
         "check 1.4 expect 1"
         "check 1.4 expect 1 within .5"
         "check 1.4 expect 1 within true")
   (list #t 'error #t #f 'error 'error #t 'error)
   "Calling check in many ways")

  (report-test-results))
