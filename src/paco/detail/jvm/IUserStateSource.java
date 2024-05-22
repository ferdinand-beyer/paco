package paco.detail.jvm;

public interface IUserStateSource extends ICharSource {
    
    long modCount();
    
    void backtrackModified(ISourceMark mark);

    Object getUserState();

    void setUserState(Object userState);
}
