package graphics;

public class Posn {
  public int x;
  public int y;

  public Posn(int x, int y) {
    this.x = x;
    this.y = y;
  }

  public boolean equals(Object p) {
    return p instanceof Posn && ((Posn) p).x==this.x && ((Posn) p).y == this.y;
  }
}