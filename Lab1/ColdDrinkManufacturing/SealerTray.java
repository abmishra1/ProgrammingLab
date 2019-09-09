import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class SealerTray {
    private int bottle1Count;
    private int bottle2Count;
    private int nextDelivery;
    private Lock lock; 

    public SealerTray(int newBottle1Count, int newBottle2Count) {
        bottle1Count = newBottle1Count;
        bottle2Count = newBottle2Count;
        nextDelivery = 0;
        lock = new ReentrantLock();
    }

    public void acquireLock() {
        lock.lock();
    }

    public void releaseLock() {
        lock.unlock();
    }

    // public boolean isEmpty() {
    //     return (bottle1Count == 0 && bottle2Count == 0);
    // }

    private boolean isBottleAvailable(int bottleType) {
        if (bottleType == 1) {
            return (bottle1Count > 0);
        }
        return (bottle2Count > 0);
    }

    private void decrementBottleCount(int bottleType) {
        if (bottleType == 1) {
            bottle1Count--;
        }
        else {
            bottle2Count--;
        }
        return;
    } 

    public int takeBottle(int bottleType, int currentTime) {
        if (!isBottleAvailable(bottleType)) {
            int otherBottleType = (bottleType + 1) % 2;
            if (!isBottleAvailable(otherBottleType)) {
                return -1;
            }
            nextDelivery = currentTime + 3;
            decrementBottleCount(otherBottleType);
            return otherBottleType;
        }
        nextDelivery = currentTime + 3;
        decrementBottleCount(bottleType);
        return bottleType;
    }

    public boolean isFull() {
        return (bottle1Count + bottle2Count >= 2);
    }

    public boolean storeBottle(int bottleType) {
        if (isFull()) return false;
        if (bottleType == 1) {
            bottle1Count++;
        }
        else {
            bottle2Count++;
        }
        return true;
    }

    public int getNextDelivery() {
        return nextDelivery;
    }

    public void printTray() {
        System.out.println("B1 in Sealer's tray: " + bottle1Count);
        System.out.println("B2 in Sealer's tray: " + bottle2Count);
    }
}