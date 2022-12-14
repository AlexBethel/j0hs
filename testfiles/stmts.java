import functional.Consumer;

public class LinkedList {
    LinkedList next;
    Object data;

    public void init(Object data) {
        this.next = null;
        this.data = data;
    }

    public void forEach(Consumer c) {
        LinkedList cur;
        cur = this;
        while (cur != null) {
            c.consume(cur.data);
            cur = cur.next;
        }
    }
}
