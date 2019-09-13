// importing libraries
import java.util.concurrent.locks.*;


// The object of this class represents an instance of godown
public class Godown {

    // variable to store number of bottle1 in godown
    private int bottle1Count;
    // variable to store number of bottle2 in godown
    private int bottle2Count;
    // lock that needs to be obtained before updating godown value for synchronization
    private Lock storageLock;

    // Constructor to initialize member variables
    public Godown() {
        bottle1Count = 0;
        bottle2Count = 0;
        storageLock = new ReentrantLock();
    }

    /* This function stores the bottle in the godown by
   incrementing the bottle count in the godown.
   A lock is applied to achieve synchronization so that at a time
   only one unit access the tray at time */
    public boolean storeBottle(int bottleType) {
        // accquire lock
        storageLock.lock();

        // increment corresponding bottle type count
        if (bottleType == 1) {
            bottle1Count++;
        }
        else {
            bottle2Count++;
        }

        // release lock and return storage successful
        storageLock.unlock();
        return true;
    }

    // This function returns bottle count of requested bottle type
    public int getBottleCount(int bottleType) {
        // if bottletype is 1 return count of bottle 1 else of bottle 2
        if (bottleType == 1) {
            return bottle1Count;
        }
        else {
            return bottle2Count;
        }
    }
}