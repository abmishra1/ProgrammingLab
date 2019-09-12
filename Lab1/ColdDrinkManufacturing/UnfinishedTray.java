import java.util.concurrent.locks.*;

public class UnfinishedTray {
    private int bottle1Count;
    private int bottle2Count;
    private Lock retrievalLock; 

    public UnfinishedTray(int newBottle1Count, int newBottle2Count) {
        bottle1Count = newBottle1Count;
        bottle2Count = newBottle2Count;
        retrievalLock = new ReentrantLock();
    } 

    private void decrementBottleCount(int bottleType) {
        if (bottleType == 1) {
            bottle1Count -= 1;
        }
        else {
            bottle2Count -= 1;
        }
        return;
    } 

    private boolean isBottleAvailable(int bottleType) {
        if (bottleType == 1) {
            return (bottle1Count > 0);
        }
        return (bottle2Count > 0);
    }

    public int takeBottle(int bottleType) {
        retrievalLock.lock();
        if (!isBottleAvailable(bottleType)) {
            int otherBottleType = (bottleType + 1) % 2;
            if (!isBottleAvailable(otherBottleType)) {
                return -1;
            }
            decrementBottleCount(otherBottleType);
            return otherBottleType;
        }
        decrementBottleCount(bottleType);
        retrievalLock.unlock();
        return bottleType;
    }
}