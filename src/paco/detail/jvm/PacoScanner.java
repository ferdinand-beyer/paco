package paco.detail.jvm;

import java.util.Objects;
import java.util.regex.MatchResult;
import java.util.regex.Pattern;

public class PacoScanner implements LineTrackingScanner, UserStateScanner {

    protected final Scanner scanner;

    private Object userState;
    private long modCount;

    protected PacoScanner(Scanner scanner, Object userState) {
        this.scanner = Objects.requireNonNull(scanner);
        this.userState = userState;
        this.modCount = 0L;
    }

    public static PacoScanner of(Scanner scanner, Object userState) {
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

    @Override
    public int skip() {
        return untrackedSkip();
    }

    @Override
    public int skip(int n) {
        return untrackedSkip(n);
    }

    protected static final class State implements ScannerState {

        public final ScannerState scannerState;
        public final long modCount;
        public final Object userState;

        public State(ScannerState scannerState, long modCount, Object userState) {
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
    public final ScannerState state() {
        return new State(scanner.state(), modCount, userState);
    }

    @Override
    public final boolean inState(ScannerState state) {
        return modCount == ((State) state).modCount;
    }

    @Override
    public void backtrack(ScannerState state) {
        final State s = (State) state;
        scanner.backtrack(s.scannerState);
        modCount = s.modCount;
        userState = s.userState;
    }

    protected final CharScanner charScanner() {
        return (CharScanner) scanner;
    }

    @Override
    public final char peekChar() {
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
    public final boolean matches(CharPredicate pred) {
        return charScanner().matches(pred);
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
    public int skipCharsWhile(CharPredicate pred) {
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
    public void backtrackModified(ScannerState state) {
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

        WithLineTracking(Scanner scanner, Object userState) {
            super(scanner, userState);
            this.lineTracker = new LineTracker();
        }

        @Override
        public final long position() {
            return lineTracker.position(scanner.index());
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
        public final int skipCharsWhile(CharPredicate pred) {
            return modifiedUnlessZero(lineTracker.skipCharsWhile(charScanner(), pred));
        }

        @Override
        public String readString(int n) {
            final String s = charScanner().peekString(n);
            if (s != null) {
                modifiedUnlessZero(lineTracker.skip(charScanner(), s.length()));
            }
            return s;
        }
    }
}
