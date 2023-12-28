package paco.impl;

import clojure.lang.IFn;

public interface IScanner {
    long index();
    boolean atEnd();
    Object peekToken();
    boolean matchesToken(Object token);
    long skip();
    long skip(long n);
    long skipWhile(IFn pred);
    long seek(long index);
}
