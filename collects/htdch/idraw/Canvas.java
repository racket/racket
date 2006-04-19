package idraw;
        
public class Canvas {
  private int width = 0; 
  private int height = 0; 

  public Canvas(int width, int height) { 
   this.width = width; 
   this.height = height; 
  }
    
  // these two are cheats: 
  public native void copy();
  public native void stop();
    // I need to figure out how to accomplish these two things, especially stop,
    // directly at the Scheme level w/o going thru the Java layer.
    // BUG: this is actually a bug in ProfessorJ. Making these protected should 
    // work just fine. Indeed, leaving off the access control qualifier should 
    // work, too. (That's package protected.) 
     
  public native void show();
  public native void close();
  public native void drawCircle(Posn p, int r, Color c);
  public native void drawDisk(Posn p, int r, Color c);
  public native void drawRect(Posn p, int width, int height, Color c);
  public native void drawLine(Posn p0, Posn p1, Color c);
  public native void drawString(Posn p, String s);
  public native void clearCircle(Posn p, int r, Color c);
  public native void clearDisk(Posn p, int r, Color c);
  public native void clearRect(Posn p, int width, int height, Color c);
  public native void clearLine(Posn p0, Posn p1, Color c);
}    
