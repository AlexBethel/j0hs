public class CodegenTest {
    public int main() {
        if (2 + 2 == 4) {
            (5 + 6 + 7).print();
        }
        return 0;
    }
}

public class int {
    public int __add__(int other) extern int_add;
    public void print() extern int_print;
}
