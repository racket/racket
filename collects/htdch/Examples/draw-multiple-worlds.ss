import draw.*;
import colors.*;
import geometry.*;

class SW extends World {
 int x = 50; 
 int y;
 int low; 
 Color c;
 Color white = new White();
 
 SW(int y, int low, Color c) { this.y = y; this.low = low; this.c = c; }

 boolean go() { return this.bigBang(100,100,.1); }
 
 World onTick() { 
  if (y >= low)
    return endOfWorld();
  else
   return new SW(this.y+1,this.low,this.c); 
 }

 World onKeyEvent(String ke) { return this; }

 boolean draw() { return this.theCanvas.drawDisk(new Posn(this.x,this.y),3,c); }
 
 boolean erase() { return this.theCanvas.drawRect(new Posn(0,0),100,100,white); }
}

class Example {
  SW sw1 = new SW(10,40,new Red());
  SW sw2 = new SW(10,90,new Green());
  boolean tst1 = sw1.go();
  boolean tst2 = sw2.go();
  String result = "two worlds, green goes to bottom, red goes to center";
}