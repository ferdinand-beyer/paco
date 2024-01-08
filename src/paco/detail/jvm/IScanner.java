package paco.detail.jvm;

public interface IScanner {

    int index();

    boolean isEnd();

    Object peek();

    int skip();

    int skip(int n);

    IScannerState state();

    boolean inState(IScannerState state);

    void backtrack(IScannerState state);
}
