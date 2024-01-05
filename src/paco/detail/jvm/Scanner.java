package paco.detail.jvm;

public interface Scanner {
    int index();

    boolean isEnd();

    Object peek();

    int skip();

    int skip(int n);

    ScannerState state();

    boolean inState(ScannerState state);

    void backtrack(ScannerState state);
}
