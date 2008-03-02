import draw.*;
import colors.*;
import geometry.*;

class Example {
 Canvas create(int w, int h, Color bg) { 
   Canvas c = new Canvas(w,h);
   boolean tstC = c.show();
   boolean tstCDraw = c.drawRect(new Posn(0,0),w,h,bg);
   return c; 
 }
}

class Example1 extends Example {
 Canvas c = create(100,200,new Black());
 Canvas d = create(200,100,new White());
 
 boolean tst = c.close();

 String result = "the white 200 x 100 canvas is visible; the black one has disappeared";
}

class Example2 extends Example {
 Canvas c = create(100,200,new Blue());
 Canvas d = create(200,100,new Red());
 
 boolean tst = d.close();

 String result = "the blue canvas is visible; the green one has disappeared";
}

class Example3 extends Example {
 Canvas c = create(100,100,new Green());
 String sc = "a green 100 x 100 canvas pops up";

 Canvas d = create(200,200,new Black());
 String sd = "a black 200 x 200 canvas pops up";
 
 boolean tst = c.close();
 String st = "the green screen has disappeared";
 boolean tst2 = c.show();
 String result = "the black canvas is visible; a second white one (of 100 x 100) is visible";
}

