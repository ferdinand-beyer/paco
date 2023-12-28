package paco.impl;

import java.util.Arrays;

public final class LineTracker {

    private static final long NEWLINE = (long) '\n';
    private static final long RETURN = (long) '\r';

    private long[] lineStarts;
    private int size;

    private long maxIndex;

    public LineTracker() {
        lineStarts = new long[16];
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

    private void push(long start) {
        ensureCapacity(size + 1);
        lineStarts[size++] = start;
    }

    public boolean track(long index, long ch) {
        return track(index, ch, -1L);
    }

    public boolean track(long index, long ch1, long ch2) {
        if (index > maxIndex) {
            maxIndex = index;
            if (ch1 == NEWLINE || (ch1 == RETURN && ch2 != NEWLINE)) {
                push(index + 1);
                return true;
            }
        }
        return false;
    }

    public long skip(ICharScanner scanner) {
        track(scanner.index(), scanner.peekChar());
        return scanner.skip();
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
