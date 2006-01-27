package graphics;

public class LargerSeq extends CommandSequence {
  Command first;
  CommandSequence rest;
  public LargerSeq(Command f, CommandSequence r) {
    first = f; rest = r;
  }
  void drawAll( View v) {
    v.drawToCanvas(first);
    rest.drawAll(v);
  }
  CommandSequence rev( CommandSequence acc ) {
    return rest.rev( new LargerSeq(first, acc));
  }
}
