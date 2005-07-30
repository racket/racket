package draw;
        
public class Canvas {

     public native boolean start(int width, int height);

     public native boolean stop();

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
