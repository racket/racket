package draw;

import geometry.*;
import colors.*;
        
public class Canvas {
    private int width = 0; 
    private int height = 0; 

    /** 
     *@author Matthias Felleisen, Kathy Gray 
     *@param width positive int, the width of the visible canvas 
     *@param height positive int, the height of the visible canvas 
     */ 
    public Canvas(int width, int height) { 
	this.width = width; 
	this.height = height; 
    }
    
    // these two are cheats: 
    protected native boolean copy();
    protected native boolean stop();
    // I need to figure out how to accomplish these two things, especially stop,
    // directly at the Scheme level w/o going thru the Java layer.
     
    private boolean showing = false; 

    /** 
     *@return  true, if it can display a white canvas 
     *The method initializes the canvas to a white area, 
     *enables the drawing methods, and finally displays the canvas. If it
     *succeeds, it produces <code>true</code>. Invoking the method a second
     *time without calling <code>close</code> before has no effect.  
     */
    public boolean show() { 
	if (!showing) {
	    xshow();
	    showing = true; 
	}
	return true; 
    }

    
    /** 
     *@return  true, if it can hide the canvas 
     *The method hides the canvas and erases the current content. 
     */
    public boolean close() { 
	xclose(); 
	showing = false; 
	return true; 
    }
	    
    public native boolean xshow();
    public native boolean xclose();

    /** 
     *@param p the center of the circle 
     *@param r its radius
     *@param c its outline color 
     *@return true, if it can draw the circle into this canvas 
     */
    public native boolean drawCircle(Posn p, int r, IColor c);

    /** 
     *@param p the center of the disk 
     *@param r its radius
     *@param c its fill and outline color 
     *@return true, if it can draw the disk into this canvas 
     */
    public native boolean drawDisk(Posn p, int r, IColor c);

    /** 
     *@param p the upper left of the rectangle 
     *@param width positive int 
     *@param height positive int 
     *@param c its outline color 
     *@return true, if it can draw the rectangle into this canvas 
     */
    public native boolean drawRect(Posn p, int width, int height, IColor c);

    /** 
     *@param p0 the first point on the line 
     *@param p1 the second point on the line 
     *@param c its color 
     *@return true, if it can draw the line into this canvas 
     */
    public native boolean drawLine(Posn p0, Posn p1, IColor c);

    /** 
     *@param p the position of the baseline of the string 
     *@param s the message to be drawn 
     *@return true, if it can draw the string into this canvas 
     */
    public native boolean drawString(Posn p, String s);

}    
