/*  Let's have a second canvas into which the world can draw. 
    See whether it interferes. 
*/

import draw.*;
import colors.*;
import geometry.*;

// can I close the canvas of a world? 
class SW extends World {
 int x = 50; 
 int y;
 AColor red = new Red();
 AColor white = new White();
 
 SW(int v) { y = v; }

 boolean go() { return this.bigBang(100,100,.1); }
 
 World onTick() { return new SW(this.y+1); }

 World onKeyEvent(String ke) { 
  if (ke.equals("s"))
   return this.endOfWorld("the end");
  else if (ke.equals("*"))
   return new SW(99);
  else 
   return this; 
 }

 boolean draw() { return this.theCanvas.drawDisk(new Posn(this.x,this.y),3,red); }
 
 boolean erase() { return this.theCanvas.drawRect(new Posn(0,0),100,100,white); }
}

class Example {
  SW sw = new SW(10);
  boolean tst1 = sw.go();
}
