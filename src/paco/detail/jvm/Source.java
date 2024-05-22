package paco.detail.jvm;

import java.io.IOException;
import java.util.Objects;
import java.util.regex.MatchResult;
import java.util.regex.Pattern;

public class Source implements ILineTrackingSource, IUserStateSource {

    protected final ISource source;

    private Object userState;
    protected long modCount;

    protected Source(ISource source, Object userState) {
        this.source = Objects.requireNonNull(source);
        this.userState = userState;
        this.modCount = 0L;
    }

    public static Source of(ISource source, Object userState) {
        return new Source(source, userState);
    }

    public static Source of(String input, Object userState, boolean enableLineTracking) {
        final StringSource source = new StringSource(input);
        if (enableLineTracking) {
            return new WithLineTracking(source, userState);
        }
        return of(source, userState);
    }

    @Override
    public final int index() {
        return source.index();
    }

    @Override
    public final boolean atEnd() {
        return source.atEnd();
    }

    @Override
    public final Object peek() {
        return source.peek();
    }

    protected final int modifiedUnlessZero(int k) {
        if (k != 0) {
            modCount++;
        }
        return k;
    }

    protected final int modifiedUnlessNegative(int k) {
        if (k >= 0) {
            modCount++;
        }
        return k;
    }

    @Override
    public int skip() {
        return untrackedSkip();
    }

    @Override
    public int skip(int n) {
        return untrackedSkip(n);
    }

    protected static final class Mark implements ISourceMark {

        public final ISourceMark sourceMark;
        public final long modCount;
        public final Object userState;

        public Mark(ISourceMark sourceMark, long modCount, Object userState) {
            this.sourceMark = sourceMark;
            this.modCount = modCount;
            this.userState = userState;
        }

        @Override
        public int index() {
            return sourceMark.index();
        }

        @Override
        public void close() throws IOException {
            sourceMark.close();
        }
    }

    @Override
    public final ISourceMark mark() {
        return new Mark(source.mark(), modCount, userState);
    }

    @Override
    public final boolean atMark(ISourceMark mark) {
        return modCount == ((Mark) mark).modCount;
    }

    @Override
    public void backtrack(ISourceMark state) {
        final Mark s = (Mark) state;
        source.backtrack(s.sourceMark);
        modCount = s.modCount;
        userState = s.userState;
    }

    protected final ICharSource charSource() {
        return (ICharSource) source;
    }

    @Override
    public final int peekChar() {
        return charSource().peekChar();
    }

    @Override
    public final String peekString(int n) {
        return charSource().peekString(n);
    }

    @Override
    public String readString(int n) {
        final String s = charSource().readString(n);
        if (s != null && !s.isEmpty()) {
            modCount++;
        }
        return s;
    }

    @Override
    public final boolean satisfies(ICharPredicate pred) {
        return charSource().satisfies(pred);
    }

    @Override
    public final boolean matchesString(String s) {
        return charSource().matchesString(s);
    }

    @Override
    public final boolean matchesStringIgnoreCase(String s) {
        return charSource().matchesStringIgnoreCase(s);
    }

    @Override
    public final MatchResult matchRegex(Pattern re) {
        return charSource().matchRegex(re);
    }

    @Override
    public int readCharWhen(ICharPredicate pred) {
        return modifiedUnlessNegative(charSource().readCharWhen(pred));
    }

    @Override
    public int skipCharsWhile(ICharPredicate pred) {
        return modifiedUnlessZero(charSource().skipCharsWhile(pred));
    }

    @Override
    public final String readFrom(ISourceMark mark) {
        return charSource().readFrom(((Mark) mark).sourceMark);
    }

    @Override
    public final long modCount() {
        return modCount;
    }

    @Override
    public void backtrackModified(ISourceMark mark) {
        source.backtrack(((Mark) mark).sourceMark);
        modCount++;
        userState = ((Mark) mark).userState;
    }

    @Override
    public final Object getUserState() {
        return userState;
    }

    @Override
    public final void setUserState(Object userState) {
        if (userState != this.userState) {
            this.userState = userState;
            modCount++;
        }
    }

    @Override
    public final long position() {
        return position(source.index());
    }

    @Override
    public long position(int index) {
        return index;
    }

    @Override
    public final int untrackedSkip() {
        return modifiedUnlessZero(source.skip());
    }

    @Override
    public final int untrackedSkip(int n) {
        return modifiedUnlessZero(source.skip(n));
    }

    private static final class WithLineTracking extends Source {

        private final LineTracker lineTracker;

        WithLineTracking(ISource source, Object userState) {
            super(source, userState);
            this.lineTracker = new LineTracker();
        }

        @Override
        public final long position(int index) {
            return lineTracker.position(index);
        }

        @Override
        public final int skip() {
            return modifiedUnlessZero(lineTracker.skip(charSource()));
        }

        @Override
        public final int skip(int n) {
            return modifiedUnlessZero(lineTracker.skip(charSource(), n));
        }

        @Override
        public final String readString(int n) {
            final String s = charSource().peekString(n);
            if (s != null) {
                modifiedUnlessZero(lineTracker.skip(charSource(), s.length()));
            }
            return s;
        }

        @Override
        public final int readCharWhen(ICharPredicate pred) {
            final ICharSource source = charSource();
            final int ch = source.readCharWhen(pred);
            if (ch >= 0) {
                modCount++;
                lineTracker.track(source.index() - 1, ch, source.peekChar());
            }
            return ch;
        }

        @Override
        public final int skipCharsWhile(ICharPredicate pred) {
            return modifiedUnlessZero(lineTracker.skipCharsWhile(charSource(), pred));
        }
    }
}
