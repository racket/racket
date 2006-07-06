package draw;

import geometry.*;
import colors.*;
        
public class Canvas {
  private int width = 0; 
  private int height = 0; 

  public Canvas(int width, int height) { 
   this.width = width; 
   this.height = height; 
  }
    
  // these two are cheats: 
  protected native boolean copy();
  protected native boolean stop();
    // I need to figure out how to accomplish these two things, especially stop,
    // directly at the Scheme level w/o going thru the Java layer.
     
  public native boolean show();
  public native boolean close();
  public native boolean drawCircle(Posn p, int r, Color c);
  public native boolean drawDisk(Posn p, int r, Color c);
  public native boolean drawRect(Posn p, int width, int height, Color c);
  public native boolean drawLine(Posn p0, Posn p1, Color c);
  public native boolean drawString(Posn p, String s);
  public native boolean clearCircle(Posn p, int r, Color c);
  public native boolean clearDisk(Posn p, int r, Color c);
  public native boolean clearRect(Posn p, int width, int height, Color c);
  public native boolean clearLine(Posn p0, Posn p1, Color c);
}    
