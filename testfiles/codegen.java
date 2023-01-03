public class CodegenTest {
    public int main() {
        int i;
        int a = 0, b = 1, c;
        for (i = 0; i != 10; i++) {
            a.print();
            c = a + b;
            a = b;
            b = c;
        }
        return 0;
    }
}

public class int {
    public int __add__(int other) extern int_add;
    public void print() extern int_print;
}

public class boolean {
    public boolean __not__() extern boolean_not;
}
