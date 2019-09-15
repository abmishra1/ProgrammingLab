/* 
    Authors' Name : Abhinav Mishra, Nitin Kedia
*/
// importing libraries
import java.util.concurrent.locks.*;

// Class to represent tray present in the packaging unit
public class PackagerTray {
    // variable to store bottle 1 count
    private int bottle1Count;
    // variable to store bottle 2 count
    private int bottle2Count;
    // lock on trays to implement synchronization
    private Lock lock;


    // constructor to initialize member variables
    public PackagerTray(int newBottle1Count, int newBottle2Count) {
        bottle1Count = newBottle1Count;
        bottle2Count = newBottle2Count;
        lock = new ReentrantLock();
    }

    // function to acquire lock
    private void acquireLock() {
        lock.lock();
    }

    // function to release lock
    private void releaseLock() {
        lock.unlock();
    }

    // function to check if tray is empty (no bottle1 or bottle2 present)
    public boolean isEmpty() {
        return (bottle1Count == 0 && bottle2Count == 0); 
    }

    // function to check if a particular bottle type is available in the tray
    private boolean isBottleAvailable(int bottleType) {
        if (bottleType == 1) {
            return (bottle1Count > 0);
        }
        return (bottle2Count > 0);
    }

    // function to decrement the bottle count of a particular type
    private void decrementBottleCount(int bottleType) {
        if (bottleType == 1) {
            bottle1Count--;
        }
        else bottle2Count--;
    } 

     /* This function takes the bottle preference from the unit and
  returns that bottle type is present else return other bottle types
  A lock is applied to achieve synchronization so that at a time
  only one unit access the tray at a time. */
    public int takeBottle(int bottleType) {
        // acquire lock
        acquireLock();
        /* if requested bottle type is not available check if the
            the other type is available */
        if (!isBottleAvailable(bottleType)) {
            int otherBottleType = (bottleType + 1) % 2;

            // If no bottle is available return -1 after releasing lock
            if (!isBottleAvailable(otherBottleType)) {
                releaseLock();
                return -1;
            }
            /* else decrement the count of other bottle type present and
           return after releasing the lock*/
            decrementBottleCount(otherBottleType);
            releaseLock();
            return otherBottleType;
        }
        /* else decrement number of bottle type present and
           return after releasing the lock */
        decrementBottleCount(bottleType);
        releaseLock();
        return bottleType;
    }

    // function to check if tray of bottle1 is full or not
    private boolean isBottle1TrayFull(){
        return bottle1Count >= 2;
    }

    // function to check if tray of bottle2 is full or not
    private boolean isBottle2TrayFull(){
        return bottle2Count >= 3;
    }

     /*This function is called to store a sealed bottle in the corresponding tray
        if the tray is not full else it returns false
        A lock is applied to achieve synchronization so that at a time
        only one unit access the tray at time */
    public boolean storeBottle(int bottleType) {

        // acquire lock
        acquireLock();
 
        if (bottleType == 1) {
            // if bottle type is 1 and its tray is full return false after releasing lock
            if (isBottle1TrayFull()) {
                releaseLock();
                return false;
            }
            // else increment bottle1 count
            bottle1Count++;
        }
        else {
            // if bottle type is 2 and its tray is full return false after releasing lock
            if (isBottle2TrayFull()) {
                releaseLock();
                return false;
            }
            // else increment bottle2 count
            bottle2Count++;
        }
        // release lock and return
        releaseLock();
        return true;
    }
}