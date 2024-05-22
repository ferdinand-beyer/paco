package paco.detail.jvm;

import clojure.lang.IFn;

@FunctionalInterface
public interface ICharPredicate {

    ICharPredicate ASCII_UPPER = inRange('A', 'Z');
    ICharPredicate ASCII_LOWER = inRange('a', 'z');
    ICharPredicate ASCII_LETTER = or(ASCII_LOWER, ASCII_UPPER);
    ICharPredicate UPPER = Character::isUpperCase;
    ICharPredicate LOWER = Character::isLowerCase;
    ICharPredicate LETTER = Character::isLetter;
    ICharPredicate WHITESPACE = Character::isWhitespace;
    ICharPredicate ISO_CONTROL = Character::isISOControl;
    ICharPredicate DIGIT = inRange('0', '9');
    ICharPredicate HEX = or(DIGIT, or(inRange('a', 'f'), inRange('A', 'F')));
    ICharPredicate OCTAL = inRange('0', '7');

    boolean test(char ch);

    static ICharPredicate of(Object x) {
        if (x instanceof ICharPredicate) {
            return (ICharPredicate) x;
        }
        if (x instanceof IFn) {
            return of((IFn) x);
        }
        throw new IllegalArgumentException("cannot coerce to ICharPredicate");
    }

    static ICharPredicate of(IFn f) {
        // 1-arg fn hinted with ^long
        if (f instanceof IFn.LO) {
            return ch -> {
                final Object ret = ((IFn.LO) f).invokePrim((long) ch);
                return ret != null && ret != Boolean.FALSE;
            };
        }
        // requires boxing
        return ch -> {
            final Object ret = f.invoke(ch);
            return ret != null && ret != Boolean.FALSE;
        };
    }

    static ICharPredicate not(ICharPredicate p) {
        return ch -> !p.test(ch);
    }

    static ICharPredicate and(ICharPredicate p1, ICharPredicate p2) {
        return ch -> p1.test(ch) && p2.test(ch);
    }

    static ICharPredicate or(ICharPredicate p1, ICharPredicate p2) {
        return ch -> p1.test(ch) || p2.test(ch);
    }

    static ICharPredicate equals(char ch) {
        return c -> ch == c;
    }

    static ICharPredicate notEquals(char ch) {
        return c -> ch != c;
    }

    static ICharPredicate among(String chars) {
        return ch -> chars.indexOf(ch) >= 0;
    }

    static ICharPredicate notAmong(String chars) {
        return ch -> chars.indexOf(ch) < 0;
    }

    static ICharPredicate inRange(char min, char max) {
        return ch -> ch >= min && ch <= max;
    }

    static ICharPredicate notInRange(char min, char max) {
        return ch -> ch < min || ch > max;
    }
}
