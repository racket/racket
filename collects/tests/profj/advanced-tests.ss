(module advanced-tests mzscheme
  (require "profj-testing.ss")
  (require profj/libs/java/lang/String)
  
  (prepare-for-tests "Advanced")
  
  ;;Execution tests without errors
  
  (execute-test
   "interface mustbepublic {
      int a();
      int b();
    }
    class musthavepublic implements mustbepublic {
      public int a() { return 3; }
      public int b() { return 5; }
    }" 'advanced #f "public implementation of an interface"
   )
  
  (execute-test
   "class OneC { }
    class TwoC extends OneC { }
    class ThreeC extends TwoC { }
    class Overload {
      int num(OneC o) { return 1; }
      int num(TwoC o) { return 2; }
      int t() { return num(new ThreeC()); }
    }
   " 'advanced #f "Overloading resolution")
  
  (execute-test
   "class Checkclass { }
    class ExampleCheck {
      boolean t1 = check new Checkclass[10] expect new Checkclass[10];
      boolean t2 = check (new int[3])[1] expect 0;
    }"
   'advanced #f "check expressions")
    
  (execute-test
   "class Blah {
	Blah () {}
	int addUp (int top) {
		int answer = 0;
		int counter = 1;
		while (counter <= top) {
			answer += counter;
			++counter;
			}
		return answer;
		}
	}"
   'advanced #f "while loop with statements after")
  
  (execute-test
   "interface Abs { int a( int x); }
    abstract class Babs implements Abs { }
   "
   'advanced #f "abs. class with interface not all impl., with args")
  
  (execute-test
   "public class Xprivate {
     private int x() { return 3; }
    }"
   'advanced #f "Class with private method")
  
  (execute-test
   "public class Something {
      private int x;
      public boolean equals( Object o ) {
        return (o instanceof Something) && x == ((Something) o).x;
      }
    }"
   'advanced
   #f
   "Correct overriding of equals")
  
  (execute-test
   "public interface F {
     int f();
    }
    public class P implements F {
     private int g;
     public int f() { return g; }
   }" 'advanced #f "Correct implementing of public interface")
      
  (execute-test
   "public class Statics {
     public static int[] a;
     public static boolean b;
     private static boolean c;

     public static void main( String[] args) {
        return;
     }
     public static int getA( int pos) {
       return a[pos];
     }
    }" 'advanced #f "Class containing several static members")
  
  (execute-test
   "public class Inits {
     private int f;
     private boolean condition;
     
     public Inits() { }
     
     { if (condition) 
        f = 4;
       else
        f = 3;
     }
    }"
   'advanced #f "Class containing inits")
  
  (execute-test
   "class Xfinal {
     final int x() { return 4; }
   }"
   'advanced #f "Class with final method")
  
  (execute-test
   "class Xoverload {
     int x() { return 3; }
     int x( int y ) { return y; }
   }" 'advanced #f "Class with overloaded methods")
  
  (execute-test
   "class Ret {
     boolean rets() {
      if (true)
        return true;
      return false;
      }
     }"
   'advanced #f "If with no else, reachable return")
  
  ;;Execution tests with errors

  (execute-test
   "interface a { int a(); }
    class b implements a{ int a() { return 3; } }"
   'advanced #t "Interface implement without public"
   )
  
  (execute-test
   "class X {
     final int x = 4;
    }" 'advanced #t "Class with final field")
  
  (execute-test
   "class X {
     final int x() { return 3; }
    }
    class Y extends X {
     int x() { return 3 ; }
    }" 'advanced #t "Attempt to override a final method")
  
  (execute-test
   "class X { 
     public int x () { return 3 ; }
    }
    class Y extends X {
     int x() { return super.x() + 3; }
    }" 'advanced #t "Attempt to weaken access privlege")
  
  (execute-test
   "class X {
     { x = 3; 
     int x; }
    }" 'advanced #t "Attempt to set before named, init")
   
  (execute-test
   "class Xth {  Xth() { this(1); }
                 Xth(int i) { this(); }}"
   'advanced #t "Cyclic calls to this")
  
  (execute-test
   "class X { 
      int x() { return 3; }
      int x( int x ) { return 35; }
      int y() { return x(3,5); }
   }" 'advanced #t "Miss overload call")
  
  (execute-test
   "class X {
     int x() { return 3; }
     int x( int y, int z ) { return y; }
     int y() { return x(y()); }
   }" 'advanced #t "Miss overload call the other way")
  
  (execute-test
   "class X {
     int x() { return 3; }
     boolean x(int y) { return true; }
     int f() { return x(3); }
    }" 'advanced #t "Miss overloading")
  
  (execute-test
   "public class StatInits {
     private static int x;
     static { x = 45; }
   }" 'advanced #t "Class containing static inits")
  
  (execute-test
   "public class F {
     public f() { return 3; }
    }"
   'advanced #t "Forgotten return type after public")

  (execute-test
   "pulic class F { }"
   'advanced #t "Parse error, misspelled public")
  
  (execute-test
   "class TestClass{
     ALoObj iterFilter(){
     for (;false;){}
     }
    }/end SameAuthor"
   'advanced
   #t "Parse error check")

  (execute-test
   "class Today{
 
            int dayNumber;
            boolean meetings;
            String QoD;
 
            Today(int dayNumber, boolean meetings, String QoD)
            {
                                    this.dayNumber = dayNumber;
                                    this.meetings = meetings;
                                    this.QoD = QoD;
            }
 
            int getDayNumber()
            {
                                    return dayNumber;
            }
 
            boolean getMeetings()
            {
                                    return meetings;
            }
 
            void setMeetings()
            {
                        this.meetings = true;
            }
                                   
            String getQoD()
            {
                                    return QoD;
            }
 
}
 
 
class WeeklyPlanner{
 
            Today[] weeklyPlanner;
            int totalDays = 7;
 
            WeeklyPlanner()
            {
                                   
                                    weeklyPlanner = new Today[totalDays];
                       
                                    for(int i = 0; i < totalDays; i++)
                                    {
                                                weeklyPlanner[i] = new Today(i, False, \"\");
                                    }
            }
 
            void addMeeting(int dayNumber)
            {
                                    weeklyPlanner[dayNumber].setMeetings();   ////////<<<<<<<<Expected assignment, found ., which is not valid for a statement
            }
 
            int howManyMeetings()
            {
                                    int numberMeetings = 0;
 
                                    for(int i = 0; i < totalDays; i++)
                                    {
                                                 if (weeklyPlanner[i].getMeetings())         
                                                              numberMeetings++;
                                    }
 
                                    return numberMeetings;
            }
 as
}
 " 'advanced #t "Parsing test from student program. Should mention {")
  
  (execute-test
   "class NoRet {
     boolean noRet() {
       if (true) 
         return true;
     }
    }"
   'advanced #t "If with no else, and return in if")
  
  (execute-test
   "class Blah {
	boolean same (int[] these, int[] those) {
		int which = 0;
		while ((which < these.length) && (which < those.length)) {
			if (these[which] == those[which]) {
				which = which + 1;
				return true;
				}
			return false;
			}
		}
	}"
   'advanced #t "No reachable return -- while loop has returns")
  

  
  ;;Interaction tests, mix of right and error
  
  (interact-test
   "interface topping { }
    interface cheese extends topping { }
    interface sausage extends topping { }
    interface parm extends cheese { }
    interface base { }
    interface red extends base { }
    interface bbq extends red { }
    interface marinara extends red { }

    class Traditional implements parm, marinara { }
    class Traditional2 extends Traditional { }
    class Odd implements bbq, cheese, sausage { }

    class OverloadTest {
      int meth( topping t, base b ) { return 1; } 
      int meth( cheese t, base b) { return 2; } 
      int meth( cheese t, marinara b) { return 3; } 
      int meth( topping t, red b) { return 4; } 
      int meth( Traditional2 t, red b) { return 5; } 
   }" 'advanced
      '("OverloadTest t = new OverloadTest();"
        "t.meth(new Traditional(), new Odd()) //ambiguous"
        "t.meth(new Traditional(), new Traditional())  // 3" 
        "t.meth(new Traditional2(), new Traditional()) //ambiguous"
        "t.meth(new Odd(), new Odd()) //ambiguous"
        "t.meth(new Odd(), (red) new Odd()) //ambiguous"
        "t.meth(new Odd(), (base) new Odd()) // 2"
        "t.meth((topping) new Odd(), (base) new Odd()) // 1"
        "t.meth(new Traditional2(), new Odd()) // 5"
        "t.meth((topping) new Odd(), new Traditional()) // 4")
      '((void) error 3 error error error 2 1 5 4)
      "Overloading resolution with interfaces and interface inheritance")
  
  (interact-test
   'advanced
   '("int a = 'a';" "a" "int b;" "b = 'a';")
   '((void) 97 (void) 97) "Conversion of char to int")
  
  (interact-test 'advanced '("int a = 1;" "++a") '((void) 2) "Test of ++")
  
  (interact-test
   'advanced
   '("check (new int[12])[3] expect 0" 
     "check new int[2] expect new int[4]"
     "check new int[3] expect new int[3]"
     "String[] t = new String[3];"
     "t[2] = \"\";"
     "check new Object[3] expect t"
     "check new String[3] expect new Object[3]"
     "check new int[3][3] expect new int[3]")
   `(#t #f #t (void) ,(make-java-string "") #f #t #f)
   "Check of arrays")
  
  (interact-test
   "class Afirst {
     private int x = 10;
     public int y = x * 2;
    }
    class Bsecond {
     public int x = 10;
     private int y = x * 2;
     public int report() { return y; }
   }
   class Cthird {
     public int x = 10;
     public int y = 2 * x;
   }
   class Dfourth {
     int x = 10;
     public int y = 2 * x;
   }"
   'advanced
   '("Afirst a = new Afirst();" 
     "Bsecond b = new Bsecond();" 
     "Cthird c = new Cthird();" 
     "Dfourth d = new Dfourth();" 
     "a.y" "b.report()" "c.y" "d.y")
   '((void) (void) (void) (void) 20 20 20 20)
   "Private, etc shouldn't effect order of evaluation")
  
  (interact-test
   "public class Xp {
     private int x() { return 3; }
    }"
   'advanced '("Xp y = new Xp();" "y.x()")
   '((void) error) "Creating class with private method, and trying to access the method")

  
  (interact-test
   'advanced
   '("null.m()")
   '(error)
   "Calling a method on null")
  
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
   (list "(new int[2][])[0]")
   (list #\null)
   "multi-dimension array - not all intialized")
  
  (interact-test
   'advanced
   (list "int[] x = new int[10];" 
         "for( int i = 0; i< x.length; i++) { x[i]=i; }" "x.length" "x[5]")
   (list '(void) '(void) 10 5)
   "Array & for loop")
  
  (interact-test
   'advanced
   (list "int[3] x;" "int[] x = new int();" "x[][1]")
   (list 'error 'error 'error)
   "Array parse errors")
  
  (interact-test
   'advanced
   (list "int[] x = { 1, 3, 4, 5};" "x[4]" "x[-1]" "x[0]=true;" "x[0]=0.01;")
   (list '(void) 'error 'error 'error 'error)
   "Array access and set errors")
  
  (interact-test
   'advanced
   (list "int[] x = new int[2];" "boolean[] y = new boolean[2];" "char[] z = new char[2];" 
         "Object[] o = new Object[2];" "x[0]" "y[0]" "z[0]" "o[0]")
   (list '(void) '(void) '(void) '(void) 0 #f #\null #\null)
   "Array initialization checks")
  
  (interact-test
   'advanced
   (list "Object o = \"\";" "(String) o")
   (list '(void) 'o~f)
   "Casting to a String")
  
  (interact-test
   'advanced
   (list "\"hello\".substring(2,5)")
   (list (make-java-string "llo"))
   "Test of substring")
         
  (interact-test
   "class A2 {
      private int a;
      A2(int a) { this.a = a; }
      int g(A2 b) { return b.a; }
   }"
   'advanced
   (list "new A2(1).g(new A2(2))")
   (list 2)
   "Test of private field access")
  
  (interact-test
   'advanced
   (list "String[] a = {new String(\"hi\"),new String(\"i\")};")
   (list '(void))
   "Test of array init")
  
  (interact-test 
   'advanced
   (list "null instanceof Object")
   (list #f)
   "Test of instanceof and null")
  
  (report-test-results)
  
  )
