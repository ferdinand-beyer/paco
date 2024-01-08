package paco.detail.jvm;

public interface ILineTrackingScanner extends ICharScanner {

    long position();

    long position(int index);

    int untrackedSkip();

    int untrackedSkip(int n);    
}
