package paco.detail.jvm;

import java.util.Arrays;

public final class LineTracker {

    private static final int INITIAL_CAPACITY = 16;

    private int[] lineStarts;
    private int allocated;

    private int maxIndex;

    public LineTracker() {
        lineStarts = new int[INITIAL_CAPACITY];
        allocated = 0;
        maxIndex = -1;
    }

    private void ensureCapacity(int minCapacity) {
        final int oldCapacity = lineStarts.length;
        if (minCapacity > oldCapacity) {
            int newCapacity = oldCapacity << 1;
            lineStarts = Arrays.copyOf(lineStarts, newCapacity);
        }
    }

    private void pushLineStart(int start) {
        ensureCapacity(allocated + 1);
        lineStarts[allocated++] = start;
    }

    public boolean track(int index, char ch) {
        return track(index, ch, CharScanner.EOS);
    }

    public boolean track(int index, char ch, char nextCh) {
        if (index > maxIndex) {
            maxIndex = index;
            // When \r\n: maxIndex++?
            if (ch == '\n' || (ch == '\r' && nextCh != '\n')) {
                pushLineStart(index + 1);
                return true;
            }
        }
        return false;
    }

    public int skip(CharScanner scanner) {
        final int index = scanner.index();
        final char ch = scanner.peekChar();
        if (ch == CharScanner.EOS) {
            return 0;
        }
        final int n = scanner.skip();
        track(index, ch, scanner.peekChar());
        return n;
    }

    public int skip(CharScanner scanner, int n) {
        int skipped = 0;
        char ch = scanner.peekChar();
        while (ch != CharScanner.EOS && skipped < n) {
            int index = scanner.index();
            skipped += scanner.skip();
            char nextCh = scanner.peekChar();
            track(index, ch, nextCh);
            ch = nextCh;
        }
        return skipped;
    }

    public int skipCharsWhile(CharScanner scanner, CharPredicate pred) {
        int skipped = 0;
        char ch = scanner.peekChar();
        while (ch != CharScanner.EOS && pred.test(ch)) {
            int index = scanner.index();
            skipped += scanner.skip();
            char nextCh = scanner.peekChar();
            track(index, ch, nextCh);
            ch = nextCh;
        }
        return skipped;
    }

    private static long lineColumn(int line, int column) {
        return (((long) line) << 32) | column;
    }

    private long searchPosition(int index) {
        // -(insertion point) - 1
        final int i = Arrays.binarySearch(lineStarts, 0, allocated, index);
        if (i < -1) {
            // after a known line start
            final int line = -i - 1;
            return lineColumn(line, index - lineStarts[line - 1]);
        }
        if (i >= 0) {
            // on a line start
            return lineColumn(i + 1, 0);
        }
        // -1 => before first line start
        return lineColumn(0, index);
    }

    public long position(int index) {
        if (allocated == 0) {
            return lineColumn(0, index);
        }
        // Common case: last tracked position.
        final int lastLineStart = lineStarts[allocated - 1];
        if (index >= lastLineStart) {
            return lineColumn(allocated, index - lastLineStart);
        }
        return searchPosition(index);
    }
}
