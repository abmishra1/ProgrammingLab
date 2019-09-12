import java.util.concurrent.locks.*;

public class PackagerTray {
    private int bottle1Count;
    private int bottle2Count;
    private Lock lock;

    public PackagerTray(int newBottle1Count, int newBottle2Count) {
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
        acquireLock();
        if (!isBottleAvailable(bottleType)) {
            int otherBottleType = (bottleType + 1) % 2;
            if (!isBottleAvailable(otherBottleType)) {
                releaseLock();
                return -1;
            }
            decrementBottleCount(otherBottleType);
            releaseLock();
            return otherBottleType;
        }
        decrementBottleCount(bottleType);
        releaseLock();
        return bottleType;
    }

    private boolean isBottle1TrayFull(){
        return bottle1Count >= 2;
    }

    private boolean isBottle2TrayFull(){
        return bottle2Count >= 3;
    }

    public boolean storeBottle(int bottleType) {
        acquireLock();
        if (bottleType == 1) {
            if (isBottle1TrayFull()) {
                releaseLock();
                return false;
            }
            bottle1Count++;
        }
        else {
            if (isBottle2TrayFull()) {
                releaseLock();
                return false;
            }
            bottle2Count++;
        }
        releaseLock();
        return true;
    }

    public void printTray() {
        System.out.println("B1 in Packager's tray: " + bottle1Count);
        System.out.println("B2 in Packager's tray: " + bottle2Count);
    }
}