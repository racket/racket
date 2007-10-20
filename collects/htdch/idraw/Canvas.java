package idraw;

import colors.*;
import geometry.*;
        
public class Canvas {
    private int width = 0; 
    private int height = 0; 

    public Canvas(int width, int height) { 
	this.width = width; 
	this.height = height; 
    }
    
    // these two are cheats: 
    protected native void copy();
    protected native void stop();
    // MF: I need to figure out how to accomplish these two things, especially
    //     stop, directly at the Scheme level w/o going thru the Java layer.
     
    private boolean showing = false; 

    public void show() { 
	if (!showing) {
	    xshow();
	    showing = true; 
	}
	return ; 
    }

    public void close() { 
	xclose(); 
	showing = false; 
	return ; 
    }
	    
    public native void xshow();
    public native void xclose();
    public native void drawCircle(Posn p, int r, IColor c);
    public native void drawDisk(Posn p, int r, IColor c);
    public native void drawRect(Posn p, int width, int height, IColor c);
    public native void drawLine(Posn p0, Posn p1, IColor c);
    public native void drawString(Posn p, String s);
}    
