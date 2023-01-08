public class Main {
    public static void main() {
        Main x;
        int i = x.increment(1);
        i.print();
        return 0;
    }

    public int increment(int x) {
        return x + 1;
    }
}

public class int {
    public int __add__(int other) extern int_add;
    public void print() extern int_print;
}

public class boolean {
    public boolean __not__() extern boolean_not;
}
