import geometry.*;
import colors.*;
import idraw.*;

class A extends World {
  Posn O = new Posn(10,25);
  Posn OO = new Posn(0,0);
  IColor RED = new Red();
  String s; 
  A(String s) { 
    this.s = s; 
  }
  
  void go() {
    bigBang(50,50,1);
  }
  
  public void onTick() {
    if (s.equals("hello")) {
      this.s = "world"; }
    else {
      this.s = "hello"; }
  }
  
  public void onKeyEvent(String key) { 
    if (key.equals("x")) {
      this.s = "bye"; }
  }
  
  public void draw() {
    this.theCanvas.drawRect(OO,50,50,RED);
    this.theCanvas.drawString(O,s);
  }
}


class Examples {
  Posn p = new Posn(10,20); 
  IColor c = new Red(); 
  boolean b = check this.p.x expect 10;
  A a = new A("hello");
  Examples() { 
    a.go(); 
  }
}
