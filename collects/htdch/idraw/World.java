package idraw;
        
public abstract class World {

    public Canvas theCanvas = new Canvas(); 

    public native void bigBang(int width, int height, double s); 
    
    public native World endOfTime();

    public native World endOfWorld();

    public abstract void onTick();

    public abstract void onKeyEvent(String ke); 

    public abstract void draw();

    public abstract void erase();
}
