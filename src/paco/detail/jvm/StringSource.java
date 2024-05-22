package paco.detail.jvm;

import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class StringSource implements ICharSource {

    public final String input;
    private final int end;

    private int index;

    public StringSource(String input) {
        this.input = input;
        this.end = input.length();
        this.index = 0;
    }

    @Override
    public int index() {
        return index;
    }

    @Override
    public boolean atEnd() {
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

    private static final class Mark implements ISourceMark {

        public final int index;

        public Mark(int index) {
            this.index = index;
        }

        @Override
        public int index() {
            return index;
        }

        @Override
        public void close() {
        }
    }

    @Override
    public Mark mark() {
        return new Mark(index);
    }

    @Override
    public boolean atMark(ISourceMark mark) {
        return index == ((Mark) mark).index;
    }

    @Override
    public void backtrack(ISourceMark state) {
        this.index = ((Mark) state).index;
    }

    @Override
    public int peekChar() {
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
    public boolean satisfies(ICharPredicate pred) {
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
    public int readCharWhen(ICharPredicate pred) {
        if (index < end) {
            final char ch = input.charAt(index);
            if (pred.test(ch)) {
                ++index;
                return ch;
            }
            return MISMATCH;
        }
        return EOS;
    }

    @Override
    public int skipCharsWhile(ICharPredicate pred) {
        int i = index;
        while (i < end && pred.test(input.charAt(i))) {
            ++i;
        }
        final int n = i - index;
        index = i;
        return n;
    }

    @Override
    public String readFrom(ISourceMark mark) {
        return input.substring(mark.index(), index);
    }
}
