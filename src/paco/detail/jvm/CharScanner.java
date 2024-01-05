package paco.detail.jvm;

import java.util.regex.MatchResult;
import java.util.regex.Pattern;

public interface CharScanner extends Scanner {

    int EOS = -1;
    int NO_MATCH = -2;

    int peekChar();

    String peekString(int n);

    String readString(int n);

    boolean matches(CharPredicate pred);

    boolean matchesString(String s);

    boolean matchesStringIgnoreCase(String s);

    MatchResult matchRegex(Pattern re);

    int readCharWhen(CharPredicate pred);

    int skipCharsWhile(CharPredicate pred);

    String readFrom(int start);
}
