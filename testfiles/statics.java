public class Main {
    public static void main() {
        int i = Statics.one();
        i.print();
        return 0;
    }

    public static int one() {
        return 1;
    }
}

public class int {
    public int __add__(int other) extern int_add;
    public void print() extern int_print;
}

public class boolean {
    public boolean __not__() extern boolean_not;
}
