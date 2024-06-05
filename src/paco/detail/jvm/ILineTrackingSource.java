package paco.detail.jvm;

public interface ILineTrackingSource extends ICharSource {

    long position();

    long position(int index);

    int untrackedSkip();

    int untrackedSkip(int n);
}
