/* 
    Authors' Name : Abhinav Mishra, Nitin Kedia
*/
// importing libraries
import java.util.concurrent.locks.*;
import java.util.LinkedList; 
import java.util.Queue; 

// Class to represent tray present in the sealing unit
public class SealerTray {
    // Queue to store the packaged bottles
    private Queue<Integer> queue;
    // lock on tray to implement synchronization
    private Lock lock;


    // Constructor to initialize member variables
    public SealerTray() {
        queue = new LinkedList<>();
        lock = new ReentrantLock();
    }

    // Function to check if tray is empty or not
    private boolean isTrayEmpty() {
        return (queue.size() == 0);
    }

    /*This function returns the bottle at the front of the queue if
   the queue is not empty else it returns -1
   A lock is applied to achieve synchronization so that at a time
   only one unit access the tray at time */
    public int takeBottle() {
        // accquire lock
        lock.lock();

        // if tray is empty return -1 after relasing lock
        if (isTrayEmpty()) {
            lock.unlock();
            return -1;
        }

        // else return front of the queue after relasing lock
        else {
            int newBottle = queue.peek();
            queue.remove();
            lock.unlock();
            return newBottle;
        }
    }

    // Function to check if tray is full or not
    private boolean isTrayFull() {
        return (queue.size() == 2);
    }

    /*This function is called to store a packaged bottle in the tray
   if the queue is not full else it returns false
   A lock is applied to achieve synchronization so that at a time
   only one unit access the tray at time */
    public boolean storeBottle(int bottleType) {
        // accquire lock
        lock.lock();

        // if tray is full return false after relasing lock
        if (isTrayFull()) {
            lock.unlock();
            return false;
        }
        // else add to queue and return true after relasing lock
        else {
            queue.add(bottleType);
            lock.unlock();
            return true;
        }
    }
}