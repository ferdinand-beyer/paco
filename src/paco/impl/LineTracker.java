package paco.impl;

import java.util.Arrays;

public final class LineTracker {

    private static final long NEWLINE = (long) '\n';
    private static final long RETURN = (long) '\r';

    private static final int INITIAL_CAPACITY = 16;

    private long[] lineStarts;
    private int size;

    private long maxIndex;

    public LineTracker() {
        lineStarts = new long[INITIAL_CAPACITY];
        size = 0;
        maxIndex = 0;
    }

    private void ensureCapacity(int minCapacity) {
        final int oldCapacity = lineStarts.length;
        if (minCapacity > oldCapacity) {
            int newCapacity = oldCapacity << 1;
            lineStarts = Arrays.copyOf(lineStarts, newCapacity);
        }
    }

    private void pushLineStart(long start) {
        ensureCapacity(size + 1);
        lineStarts[size++] = start;
    }

    public boolean track(long index, long ch) {
        return track(index, ch, -1L);
    }

    public boolean track(long index, long ch, long nextCh) {
        if (index > maxIndex) {
            maxIndex = index;
            if (ch == NEWLINE || (ch == RETURN && nextCh != NEWLINE)) {
                pushLineStart(index + 1);
                return true;
            }
        }
        return false;
    }

    public long skip(ICharScanner scanner) {
        final long index = scanner.index();
        final long ch = scanner.peekChar();
        final long n = scanner.skip();
        track(index, ch, scanner.peekChar());
        return n;
    }

    public long skip(ICharScanner scanner, long n) {
        long skipped = 0;
        long ch = scanner.peekChar();
        while (ch >= 0 && skipped < n) {
            long index = scanner.index();
            skipped += scanner.skip();
            long nextCh = scanner.peekChar();
            track(index, ch, nextCh);
            ch = nextCh;
        }
        return skipped;
    }

    public long skipCharsWhile(ICharScanner scanner, CharPredicate pred) {
        long skipped = 0;
        long ch = scanner.peekChar();
        while (ch >= 0 && pred.satisfies((char) ch)) {
            long index = scanner.index();
            skipped += scanner.skip();
            long nextCh = scanner.peekChar();
            track(index, ch, nextCh);
            ch = nextCh;
        }
        return skipped;
    }

    public long[] position(long index) {
        final int i = Arrays.binarySearch(lineStarts, 0, size, index);
        long line, column;
        if (i < -1) {
            line = -i - 1;
            column = index - lineStarts[-i - 2];
        } else if (i >= 0) {
            line = i + 1;
            column = 0;
        } else {
            line = 0;
            column = index;
        }
        return new long[] { line + 1, column + 1 };
    }
}
