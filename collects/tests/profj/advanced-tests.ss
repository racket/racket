(module advanced-tests mzscheme
  (require (lib "profj-testing.ss" "profj"))
  
  (prepare-for-tests "Advanced")
  
  ;;Execution tests without errors

  (interact-test
   'advanced
   '("null.m()")
   '(error)
   "Calling a method on null")
  
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
   "class X {
     final int x() { return 4; }
   }"
   'advanced #f "Class with final method")
  
  (execute-test
   "class X {
     int x() { return 3; }
     int x( int y ) { return y; }
   }" 'advanced #f "Class with overloaded methods")
  
  ;;Execution tests with errors

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
   "class X {  X() { this(1); }
               X(int i) { this(); }}"
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
  
  ;;Interaction tests, mix of right and error
  
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
  
  (interact-test
   'advanced
   (list "int[] x = new int[10];" "for( int i = 0; i< x.length; i++) x[i]=i;" "x.length" "x[5]")
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
   (list '(void) '(void) '(void) '(void) 0 #f #\null null)
   "Array initialization checks")
  
  (report-test-results)
  
  )