package j0;

import j0.Ptr;

public class Libc {
    public int strlen(Ptr str) extern strlen;
    public int foo() {
        do(something);
    }

    public Ptr malloc(int size) extern malloc;
    public int write(int fd, Ptr buf, int count) extern write;
}
