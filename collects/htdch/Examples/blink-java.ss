import geometry.*;
import colors.*;
import draw.*;

class A extends World {
  Posn O = new Posn(10,25);
  Posn OO = new Posn(0,0);
  IColor RED = new Red();
  String s; 
  A(String s) { 
    this.s = s; 
  }
  
  boolean go() {
    return bigBang(50,50,1);
  }
  
  World onTick() {
    if (s.equals("hello")) {
      return new A("world"); }
    else {
      return new A("hello"); }
  }
  
  World onKeyEvent(String key) { 
    if (key.equals("x")) {
      return new A("bye"); }
    else {
      return this; }
  }
  
  boolean draw() {
    return this.theCanvas.drawRect(OO,50,50,RED)
        && this.theCanvas.drawString(O,s);
  }
}


class Examples {
  Posn p = new Posn(10,20); 
  IColor c = new Red(); 
  Examples() { }
  boolean b = check this.p.x expect 10;
  boolean a = new A("hello").go(); 
}
