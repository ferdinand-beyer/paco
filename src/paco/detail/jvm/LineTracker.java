package paco.detail.jvm;

import java.util.Arrays;

public final class LineTracker {

    private static final int INITIAL_CAPACITY = 16;

    private int[] lineStarts;
    private int allocated;

    private int maxIndex;

    // TODO: Take an initial position?
    // Could be just a long that can be added to our position long.
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

    public boolean track(int index, int ch) {
        return track(index, ch, ICharSource.EOS);
    }
    
    public boolean track(int index, int ch, int nextCh) {
        if (index > maxIndex) {
            maxIndex = index;
            if (ch == '\n') {
                pushLineStart(index + 1);
                return true;
            }
            if (ch == '\r') {
                if (nextCh == '\n') {
                    maxIndex++;
                    pushLineStart(index + 2);
                } else {
                    pushLineStart(index + 1);
                }
                return true;
            }
        }
        return false;
    }

    public int skip(ICharSource source) {
        final int index = source.index();
        final int ch = source.peekChar();
        if (ch < 0) {
            return 0;
        }
        final int n = source.skip();
        track(index, ch, source.peekChar());
        return n;
    }

    public int skip(ICharSource source, int n) {
        int skipped = 0;
        int ch = source.peekChar();
        while (ch >= 0 && skipped < n) {
            int index = source.index();
            skipped += source.skip();
            int nextCh = source.peekChar();
            track(index, ch, nextCh);
            ch = nextCh;
        }
        return skipped;
    }

    public int skipCharsWhile(ICharSource source, ICharPredicate pred) {
        int skipped = 0;
        int ch = source.peekChar();
        while (ch >= 0 && pred.test((char) ch)) {
            int index = source.index();
            skipped += source.skip();
            int nextCh = source.peekChar();
            track(index, ch, nextCh);
            ch = nextCh;
        }
        return skipped;
    }

    private static long lineColumn(long line, long column) {
        return (line << 32) | column;
    }

    private long searchPosition(int index) {
        final int i = Arrays.binarySearch(lineStarts, 0, allocated, index);
        if (i < -1) {
            // after a known line start
            // i := -(insertion point) - 1
            final int line = -i - 1;
            return lineColumn(line, index - lineStarts[line - 1]);
        }
        if (i >= 0) {
            // on a line start
            return lineColumn(i + 1, 0);
        }
        // -1 => before first line start (insertion point: 0)
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
