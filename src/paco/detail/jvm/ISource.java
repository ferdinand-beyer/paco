package paco.detail.jvm;

public interface ISource {

    // TODO
    //String name();

    int index();

    boolean atEnd();

    Object peek();

    int skip();

    int skip(int n);

    ISourceMark mark();

    boolean atMark(ISourceMark mark);

    void backtrack(ISourceMark mark);
}
