package paco.impl;

import clojure.lang.IFn;

@FunctionalInterface
public interface CharPredicate {
    CharPredicate ASCII_UPPER = inRange('A', 'Z');
    CharPredicate ASCII_LOWER = inRange('a', 'z');
    CharPredicate ASCII_LETTER = or(ASCII_LOWER, ASCII_UPPER);
    CharPredicate UPPER = Character::isUpperCase;
    CharPredicate LOWER = Character::isLowerCase;
    CharPredicate LETTER = Character::isLetter;
    CharPredicate WHITESPACE = Character::isWhitespace;
    CharPredicate ISO_CONTROL = Character::isISOControl;
    CharPredicate DIGIT = inRange('0', '9');
    CharPredicate HEX = or(inRange('0', '9'), or(inRange('a', 'f'), inRange('A', 'F')));
    CharPredicate OCTAL = inRange('0', '7');

    boolean satisfies(char ch);

    static CharPredicate not(CharPredicate p) {
        return (ch) -> !p.satisfies(ch);
    }

    static CharPredicate and(CharPredicate p1, CharPredicate p2) {
        return (ch) -> p1.satisfies(ch) && p2.satisfies(ch);
    }

    static CharPredicate or(CharPredicate p1, CharPredicate p2) {
        return (ch) -> p1.satisfies(ch) || p2.satisfies(ch);
    }

    static CharPredicate of(IFn f) {
        // 1-arg fn hinted with ^long
        if (f instanceof IFn.LO) {
            final IFn.LO flo = (IFn.LO) f;
            return (ch) -> {
                final Object ret = flo.invokePrim((long) ch);
                return ret != null && ret != Boolean.FALSE;
            };
        }
        // requires boxing
        return (ch) -> {
            final Object ret = f.invoke(ch);
            return ret != null && ret != Boolean.FALSE;
        };
    }

    static CharPredicate equals(char ch) {
        return (c) -> ch == c;
    }

    static CharPredicate notEquals(char ch) {
        return (c) -> ch != c;
    }

    static CharPredicate among(String chars) {
        return (ch) -> chars.indexOf(ch) >= 0;
    }

    static CharPredicate notAmong(String chars) {
        return (ch) -> chars.indexOf(ch) < 0;
    }

    static CharPredicate inRange(char min, char max) {
        return (ch) -> ch >= min && ch <= max;
    }

    static CharPredicate notInRange(char min, char max) {
        return (ch) -> ch < min || ch > max;
    }
}
