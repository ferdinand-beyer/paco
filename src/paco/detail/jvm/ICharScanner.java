package paco.detail.jvm;

import java.util.regex.MatchResult;
import java.util.regex.Pattern;

public interface ICharScanner extends IScanner {

    int EOS = -1;
    int MISMATCH = -2;

    int peekChar();

    String peekString(int n);

    String readString(int n);

    boolean satisfies(ICharPredicate pred);

    boolean matchesString(String s);

    boolean matchesStringIgnoreCase(String s);

    MatchResult matchRegex(Pattern re);

    int readCharWhen(ICharPredicate pred);

    int skipCharsWhile(ICharPredicate pred);

    String readFrom(int start);
}
