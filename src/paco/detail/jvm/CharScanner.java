package paco.detail.jvm;

import java.util.regex.MatchResult;
import java.util.regex.Pattern;

public interface CharScanner extends Scanner {

    char EOS = '\uffff';

    char peekChar();

    String peekString(int n);

    String readString(int n);

    boolean matches(CharPredicate pred);

    boolean matchesString(String s);

    boolean matchesStringIgnoreCase(String s);

    MatchResult matchRegex(Pattern re);

    int skipCharsWhile(CharPredicate pred);

    String readFrom(int start);
}
