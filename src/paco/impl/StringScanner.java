package paco.impl;

import clojure.lang.IFn;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.MatchResult;
import java.util.regex.Pattern;

public final class StringScanner implements ICharScanner {

    public final String input;
    private final int length;
    private int index;

    public StringScanner(String input) {
        this.input = input;
        this.length = input.length();
        this.index = 0;
    }

    @Override
    public long index() {
        return index;
    }

    @Override
    public boolean atEnd() {
        return index >= length;
    }

    @Override
    public Object peekToken() {
        return index < length ? input.charAt(index) : null;
    }

    @Override
    public boolean matchesToken(Object token) {
        return index < length
            && token instanceof Character
            && ((Character) token).charValue() == input.charAt(index);
    }

    @Override
    public long skip() {
        if (index < length) {
            ++index;
            return 1L; 
        }
        return 0L;
    }

    @Override
    public long skip(long n)  {
        final int idx = Math.min(index + (int) n, length);
        final int count = idx - this.index;
        this.index = idx;
        return count;
    }

    @Override
    public long skipWhile(IFn pred)  {
        return skipCharsWhile(CharPredicate.of(pred));
    }

    @Override
    public long seek(long index) {
        Objects.checkIndex(index, length);
        this.index = (int) index;
        return index;
    }

    @Override
    public long peekChar() {
        return index < length ? input.charAt(index) : -1L;
    }

    @Override
    public String peekString(long n) {
        return index < length ? input.substring(index, Math.min(index + (int) n, length)) : null;
    }

    @Override
    public boolean matchesChar(long ch) {
        return index < length && ch == (long) input.charAt(index);
    }

    @Override
    public boolean matches(CharPredicate pred) {
        return index < length && pred.satisfies(input.charAt(index));
    }

    @Override
    public boolean matchesString(String s) {
        return index < length && input.regionMatches(index, s, 0, s.length());
    }

    @Override
    public boolean matchesStringCI(String s) {
        return index < length && input.regionMatches(true, index, s, 0, s.length());
    }

    @Override
    public MatchResult match(Pattern p) {
        if (index < length) {
            final Matcher m = p.matcher(input).region(index, length);
            if (m.lookingAt()) {
                return m;
            }
        }
        return null;
    }

    @Override
    public long skipCharsWhile(CharPredicate pred)  {
        int i = index;
        while (i < length && pred.satisfies(input.charAt(i))) {
            ++i;
        }
        final int n = i - index;
        index = i;
        return n;
    }
}
