package paco.detail.jvm;

public interface IUserStateScanner extends ICharScanner {
    
    long modCount();
    
    void backtrackModified(IScannerState state);

    Object getUserState();

    void setUserState(Object userState);
}
