package graphics;

public class EmptySeq extends CommandSequence {
  public EmptySeq() { }
  void drawAll( View v) { }
  CommandSequence rev( CommandSequence acc) {
    return acc;
  }
}
