package paco.detail.jvm;

import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class StringScanner implements CharScanner {

    public final String input;
    private final int end;

    private int index;

    public StringScanner(String input) {
        this.input = input;
        this.end = input.length();
        this.index = 0;
    }

    @Override
    public int index() {
        return index;
    }

    @Override
    public boolean isEnd() {
        return index >= end;
    }

    @Override
    public Object peek() {
        return index < end ? input.charAt(index) : null;
    }

    @Override
    public int skip() {
        if (index < end) {
            ++index;
            return 1;
        }
        return 0;
    }

    @Override
    public int skip(int n) {
        final int idx = Math.min(index + n, end);
        final int count = idx - this.index;
        this.index = idx;
        return count;
    }

    private static final class State implements ScannerState {

        public final int index;

        public State(int index) {
            this.index = index;
        }

        @Override
        public int index() {
            return index;
        }
    }

    @Override
    public State state() {
        return new State(index);
    }

    @Override
    public boolean inState(ScannerState state) {
        return index == ((State) state).index;
    }

    @Override
    public void backtrack(ScannerState state) {
        this.index = ((State) state).index;
    }

    @Override
    public char peekChar() {
        return index < end ? input.charAt(index) : EOS;
    }

    @Override
    public String peekString(int n) {
        return index < end ? input.substring(index, Math.min(index + n, end)) : null;
    }

    @Override
    public String readString(int n) {
        if (index < end) {
            final String s = input.substring(index, Math.min(index + n, end));
            index += s.length();
            return s;
        }
        return null;
    }

    @Override
    public boolean matches(CharPredicate pred) {
        return index < end && pred.test(input.charAt(index));
    }

    @Override
    public boolean matchesString(String s) {
        return input.regionMatches(index, s, 0, s.length());
    }

    @Override
    public boolean matchesStringIgnoreCase(String s) {
        return input.regionMatches(true, index, s, 0, s.length());
    }

    @Override
    public MatchResult matchRegex(Pattern p) {
        if (index < end) {
            final Matcher m = p.matcher(input).region(index, end).useAnchoringBounds(false);
            if (m.lookingAt()) {
                return m;
            }
        }
        return null;
    }

    @Override
    public int skipCharsWhile(CharPredicate pred) {
        int i = index;
        while (i < end && pred.test(input.charAt(i))) {
            ++i;
        }
        final int n = i - index;
        index = i;
        return n;
    }

    @Override
    public String readFrom(int start) {
        return input.substring(start, index);
    }
}
