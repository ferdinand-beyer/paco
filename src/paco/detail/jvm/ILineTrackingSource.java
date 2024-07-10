package paco.detail.jvm;

public interface ILineTrackingSource extends ICharSource {

    static long positionAt(int line, int column) {
        return (((long) line) << Integer.SIZE) | (long) column;
    }

    static int lineIndex(long position) {
        return (int) (position >> Integer.SIZE);
    }

    static int columnIndex(long position) {
        return (int) (position & Integer.MAX_VALUE);
    }

    long position();

    long position(int index);

    int untrackedSkip();

    int untrackedSkip(int n);
}
