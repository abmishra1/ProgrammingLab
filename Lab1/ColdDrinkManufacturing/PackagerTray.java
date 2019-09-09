import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class PackagerTray {
    public int bottle1Count;
    public int bottle2Count;
    private Lock lock;

    public PackagerTray(int newBottle1Count, int newBottle2Count) {
        bottle1Count = newBottle1Count;
        bottle2Count = newBottle2Count;
        lock = new ReentrantLock();
    }

    public void acquireLock() {
        lock.lock();
    }

    public void releaseLock() {
        lock.unlock();
    }

    public boolean isEmpty() {
        return (bottle1Count == 0 && bottle2Count == 0); 
    }

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
        else bottle2Count--;
    } 

    public int takeBottle(int bottleType) {
        if (!isBottleAvailable(bottleType)) {
            int otherBottleType = (bottleType + 1) % 2;
            if (!isBottleAvailable(otherBottleType)) {
                return -1;
            }
            decrementBottleCount(otherBottleType);
            return otherBottleType;
        }
        decrementBottleCount(bottleType);
        return bottleType;
    }

    private boolean isBottle1TrayFull(){
        return bottle1Count >= 2;
    }

    private boolean isBottle2TrayFull(){
        return bottle2Count >= 3;
    }

    public boolean storeBottle(int bottleType) {
        if (bottleType == 1) {
            if (isBottle1TrayFull())
                return false;
            bottle1Count++;
        }
        else {
            if (isBottle2TrayFull())
                return false;
            bottle2Count++;
        }
        return true;
    }

    public void printTray() {
        System.out.println("B1 in Packager's tray: " + bottle1Count);
        System.out.println("B2 in Packager's tray: " + bottle2Count);
    }
}