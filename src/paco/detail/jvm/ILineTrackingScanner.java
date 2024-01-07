package paco.detail.jvm;

public interface ILineTrackingScanner extends ICharScanner {

    long position();

    int untrackedSkip();

    int untrackedSkip(int n);    
}
