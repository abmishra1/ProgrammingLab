import java.util.concurrent.locks.*;

public class Godown {
    int bottle1Count;
    int bottle2Count;
    private Lock storageLock;

    public Godown() {
        bottle1Count = 0;
        bottle2Count = 0;
        storageLock = new ReentrantLock();
    }

    public boolean storeBottle(int bottleType) {
        storageLock.lock();
        if (bottleType == 1) {
            bottle1Count++;
            return true;
        }
        bottle2Count++;
        storageLock.unlock();
        return true;
    }

    public int getBottleCount(int bottleType) {
        if (bottleType == 1) {
            return bottle1Count;
        }
        return bottle2Count;
    }
}