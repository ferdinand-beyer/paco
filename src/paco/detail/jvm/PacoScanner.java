package paco.detail.jvm;

import java.util.Objects;
import java.util.regex.MatchResult;
import java.util.regex.Pattern;

public class PacoScanner implements ILineTrackingScanner, IUserStateScanner {

    protected final IScanner scanner;

    private Object userState;
    protected long modCount;

    protected PacoScanner(IScanner scanner, Object userState) {
        this.scanner = Objects.requireNonNull(scanner);
        this.userState = userState;
        this.modCount = 0L;
    }

    public static PacoScanner of(IScanner scanner, Object userState) {
        return new PacoScanner(scanner, userState);
    }

    public static PacoScanner of(String input, Object userState, boolean enableLineTracking) {
        final StringScanner scanner = new StringScanner(input);
        if (enableLineTracking) {
            return new WithLineTracking(scanner, userState);
        }
        return of(scanner, userState);
    }

    @Override
    public final int index() {
        return scanner.index();
    }

    @Override
    public final boolean isEnd() {
        return scanner.isEnd();
    }

    @Override
    public final Object peek() {
        return scanner.peek();
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

    protected static final class State implements IScannerState {

        public final IScannerState scannerState;
        public final long modCount;
        public final Object userState;

        public State(IScannerState scannerState, long modCount, Object userState) {
            this.scannerState = scannerState;
            this.modCount = modCount;
            this.userState = userState;
        }

        @Override
        public int index() {
            return scannerState.index();
        }
    }

    @Override
    public final IScannerState state() {
        return new State(scanner.state(), modCount, userState);
    }

    @Override
    public final boolean inState(IScannerState state) {
        return modCount == ((State) state).modCount;
    }

    @Override
    public void backtrack(IScannerState state) {
        final State s = (State) state;
        scanner.backtrack(s.scannerState);
        modCount = s.modCount;
        userState = s.userState;
    }

    protected final ICharScanner charScanner() {
        return (ICharScanner) scanner;
    }

    @Override
    public final int peekChar() {
        return charScanner().peekChar();
    }

    @Override
    public final String peekString(int n) {
        return charScanner().peekString(n);
    }

    @Override
    public String readString(int n) {
        final String s = charScanner().readString(n);
        if (s != null && !s.isEmpty()) {
            modCount++;
        }
        return s;
    }

    @Override
    public final boolean satisfies(ICharPredicate pred) {
        return charScanner().satisfies(pred);
    }

    @Override
    public final boolean matchesString(String s) {
        return charScanner().matchesString(s);
    }

    @Override
    public final boolean matchesStringIgnoreCase(String s) {
        return charScanner().matchesStringIgnoreCase(s);
    }

    @Override
    public final MatchResult matchRegex(Pattern re) {
        return charScanner().matchRegex(re);
    }

    @Override
    public int readCharWhen(ICharPredicate pred) {
        return modifiedUnlessNegative(charScanner().readCharWhen(pred));
    }

    @Override
    public int skipCharsWhile(ICharPredicate pred) {
        return modifiedUnlessZero(charScanner().skipCharsWhile(pred));
    }

    @Override
    public final String readFrom(int start) {
        return charScanner().readFrom(start);
    }

    @Override
    public final long modCount() {
        return modCount;
    }

    @Override
    public void backtrackModified(IScannerState state) {
        final State s = (State) state;
        scanner.backtrack(s.scannerState);
        modCount++;
        userState = s.userState;
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
    public long position() {
        return 0L;
    }

    @Override
    public final int untrackedSkip() {
        return modifiedUnlessZero(scanner.skip());
    }

    @Override
    public final int untrackedSkip(int n) {
        return modifiedUnlessZero(scanner.skip(n));
    }

    private static final class WithLineTracking extends PacoScanner {

        private final LineTracker lineTracker;

        WithLineTracking(IScanner scanner, Object userState) {
            super(scanner, userState);
            this.lineTracker = new LineTracker();
        }

        @Override
        public final long position() {
            return lineTracker.position(charScanner().index());
        }

        @Override
        public final int skip() {
            return modifiedUnlessZero(lineTracker.skip(charScanner()));
        }

        @Override
        public final int skip(int n) {
            return modifiedUnlessZero(lineTracker.skip(charScanner(), n));
        }

        @Override
        public String readString(int n) {
            final String s = charScanner().peekString(n);
            if (s != null) {
                modifiedUnlessZero(lineTracker.skip(charScanner(), s.length()));
            }
            return s;
        }

        @Override
        public int readCharWhen(ICharPredicate pred) {
            final ICharScanner scanner = charScanner();
            final int ch = scanner.readCharWhen(pred);
            if (ch >= 0) {
                modCount++;
                lineTracker.track(scanner.index() - 1, ch, scanner.peekChar());
            }
            return ch;
        }

        @Override
        public final int skipCharsWhile(ICharPredicate pred) {
            return modifiedUnlessZero(lineTracker.skipCharsWhile(charScanner(), pred));
        }
    }
}
