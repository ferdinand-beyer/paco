package paco.impl;

import java.util.regex.MatchResult;
import java.util.regex.Pattern;

public interface ICharScanner extends IScanner {
    long peekChar();
    String peekString(long n);
    boolean matchesChar(long ch);
    boolean matches(CharPredicate pred);
    boolean matchesString(String s);
    boolean matchesStringCI(String s);
    MatchResult match(Pattern p);
    long skipCharsWhile(CharPredicate pred);
}
