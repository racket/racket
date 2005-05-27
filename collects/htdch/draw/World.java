package draw;
        
public class World {

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

     public native boolean sleepForAWhile(int s);

     public native boolean bigBang(double s); 

     public native World onTick();

     public native World onKeyEvent(String ke); 

     public native boolean draw();

     public native boolean erase();

     public native boolean endOfTime();

     public native World endOfWorld();

     public native World lastWorld();
}    
