package idraw;
        
public class Canvas {

     public native void start(int width, int height);

     public native void stop();

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
