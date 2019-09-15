/* 
    Authors' Name : Abhinav Mishra, Nitin Kedia
*/
// import libraries
import java.util.concurrent.locks.*;

// Object of this class represents an instance of common tray 
public class UnfinishedTray {
    // variable to store bottle1 count
    private int bottle1Count;
    // variable to store bottle2 count
    private int bottle2Count;
    // lock on tray to implement synchronization
    private Lock retrievalLock; 

    // constructor to initialize member variables
    public UnfinishedTray(int newBottle1Count, int newBottle2Count) {
        bottle1Count = newBottle1Count;
        bottle2Count = newBottle2Count;
        retrievalLock = new ReentrantLock();
    } 

    // function to decrement the bottle count of a particular type
    private void decrementBottleCount(int bottleType) {
        if (bottleType == 1) {
            bottle1Count -= 1;
        }
        else {
            bottle2Count -= 1;
        }
        return;
    } 

    // function to check if a particular bottle type is available in the tray
    private boolean isBottleAvailable(int bottleType) {
        if (bottleType == 1) {
            return (bottle1Count > 0);
        }
        else {
            return (bottle2Count > 0);
        }
    }

    /*This function takes the bottle preference from the unit and
   returns that bottle type is present else return other bottle type
   A lock is applied to achieve synchronization so that at a time
   only one unit access the tray at time */
    public int takeBottle(int bottleType) {
        // acquire lock
        retrievalLock.lock();

        /* if requested bottle type is not available update and
       return other type after releasing the lock */
        if (!isBottleAvailable(bottleType)) {
            int otherBottleType = (bottleType + 1) % 2;

            // if no bottle is available return -1 after releasing lock
            if (!isBottleAvailable(otherBottleType)) {
                retrievalLock.unlock();
                return -1;
            }
             /* else it means other bottle type is available update it and
                return bottle type after releasing the lock*/
            decrementBottleCount(otherBottleType);
            retrievalLock.unlock();
            return otherBottleType;
        }
         /* if requested bottle type is available update and
       return bottle type after releasing the lock*/
        decrementBottleCount(bottleType);
        retrievalLock.unlock();
        return bottleType;
    }
}