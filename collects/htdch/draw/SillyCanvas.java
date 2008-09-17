package draw;

import geometry.*;
import colors.*;

public class SillyCanvas extends Canvas {
  private int x = 20; 
  private int y = 20; 
  
  private boolean warning() {
    return super.drawString(new Posn(x,y),"This is a Dummy Canvas.");
  }
  
  SillyCanvas(int w, int h) { 
    super(w,h); 
    if ((w < x) || (h < y)) 
    throw new RuntimeException("SillyCanvas: bad size");
  }
  
  public boolean drawCircle(Posn p, int r, IColor c) {
    return super.drawCircle(p,r,c) && warning(); 
  }
  
  public boolean drawDisk(Posn p, int r, IColor c) {
    return super.drawDisk(p,r,c) && warning(); 
  }
  
  public boolean drawRect(Posn p, int width, int height, IColor c) {
    return super.drawRect(p,width,height,c) && warning(); 
  }
  
  public boolean drawLine(Posn p0, Posn p1, IColor c) {
    return super.drawLine(p0,p1,c) && warning();
  }
  
  public boolean drawString(Posn p, String s) { 
    return super.drawString(p,s) && warning(); 
  }
  
  public boolean show() {
    return super.show() && warning(); 
  }
}
