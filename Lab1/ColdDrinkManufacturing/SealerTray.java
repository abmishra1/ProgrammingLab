import java.util.concurrent.locks.*;

public class SealerTray {
    private int bottle1Count;
    private int bottle2Count;
    private Lock lock; 

    public SealerTray(int newBottle1Count, int newBottle2Count) {
        bottle1Count = newBottle1Count;
        bottle2Count = newBottle2Count;
        lock = new ReentrantLock();
    }

    private void acquireLock() {
        lock.lock();
    }

    private void releaseLock() {
        lock.unlock();
    }

    private boolean isBottleAvailable(int bottleType) {
        if (bottleType == 1) {
            return (bottle1Count > 0);
        }
        else {
            return (bottle2Count > 0);
        }
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

    public int takeBottle() {
        acquireLock();
        // give priority to bottle1
        int bottleType = 1;
        if (!isBottleAvailable(bottleType)) {
            int otherBottleType = (bottleType + 1) % 2;
            if (!isBottleAvailable(otherBottleType)) {
                releaseLock();
                return -1;
            }
            else {
                decrementBottleCount(otherBottleType);
                releaseLock();
                return otherBottleType;
            }
        }
        decrementBottleCount(bottleType);
        releaseLock();
        return bottleType;
    }

    public boolean isFull() {
        return (bottle1Count + bottle2Count >= 2);
    }

    public boolean storeBottle(int bottleType) {
        acquireLock();
        if (isFull()) {
            releaseLock();
            return false;
        }
        if (bottleType == 1) {
            bottle1Count++;
        }
        else {
            bottle2Count++;
        }
        releaseLock();
        return true;
    }

    public void printTray() {
        System.out.println("B1 in Sealer's tray: " + bottle1Count);
        System.out.println("B2 in Sealer's tray: " + bottle2Count);
    }
}