import java.util.concurrent.locks.*;
import java.util.LinkedList; 
import java.util.Queue; 

public class SealerTray {
    private Queue<Integer> queue;
    private Lock lock;

    public SealerTray() {
        queue = new LinkedList<>();
        lock = new ReentrantLock();
    }

    private boolean isTrayEmpty() {
        return (queue.size() == 0);
    }

    public int takeBottle() {
        lock.lock();
        if (isTrayEmpty()) {
            lock.unlock();
            return -1;
        }
        else {
            int newBottle = queue.peek();
            queue.remove();
            lock.unlock();
            return newBottle;
        }
    }

    private boolean isTrayFull() {
        return (queue.size() == 2);
    }

    public boolean storeBottle(int bottleType) {
        lock.lock();
        if (isTrayFull()) {
            lock.unlock();
            return false;
        }
        else {
            queue.add(bottleType);
            lock.unlock();
            return true;
        }
    }
}