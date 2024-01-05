package paco.detail.jvm;

public interface LineTrackingScanner extends CharScanner {

    long position();

    int untrackedSkip();

    int untrackedSkip(int n);    
}
