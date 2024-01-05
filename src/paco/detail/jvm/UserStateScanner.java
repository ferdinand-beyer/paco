package paco.detail.jvm;

public interface UserStateScanner extends CharScanner {
    
    long modCount();
    
    void backtrackModified(ScannerState state);

    Object getUserState();

    void setUserState(Object userState);
}
