import java.util.concurrent.TimeoutException; 
import java.util.concurrent.BrokenBarrierException; 

public class SealingUnit implements Runnable {
    public Tray trays;
    public boolean bottleReady;
    public int bottleTypeUnfinished;
    public int bottleTypeTray;
    public Bottle processingBottle;
    public ProcessingUnit processingUnitReference;
    public ColdDrinkManufacturing coldDrinkManufacturing;

    public SealingUnit(ColdDrinkManufacturing newColdDrinkManufacturing) {
        trays = new Tray(0,0);
        bottleReady = false;
        bottleTypeUnfinished = 0;
        processingBottle = null;
        coldDrinkManufacturing = newColdDrinkManufacturing;
    }
    
    public boolean isTrayFull() {
        if ((trays.bottle1 + trays.bottle2) >= 2) {
            return true;
        }
        return false;
    }

    public boolean isTrayEmpty() {
        if (trays.bottle1 == 0 && trays.bottle2 == 0) {
            return true;
        }
        return false;
    }

    public boolean storeBottle(int bottleType) {
        if (isTrayFull()) return false;
        if (bottleType == 1) {
            trays.bottle1++;
        }
        else {
            trays.bottle2++;
        }
        return true;
    }

    private boolean isBottleAvailable(int bottleType) {
        if (bottleType == 1) {
            return (trays.bottle1 > 0);
        }
        return (trays.bottle2 > 0);
    }

    private void decrementBottleCount(int bottleType) {
        if (bottleType == 1) {
            trays.bottle1--;
        }
        else trays.bottle2--;
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

    public void printTray() {
        System.out.println("B1 in Sealer's tray: " + trays.bottle1);
        System.out.println("B2 in Sealer's tray: " + trays.bottle2);
    }

    public void run() {
        while (coldDrinkManufacturing.currentTime <= coldDrinkManufacturing.stopTime) {
            // System.out.println("Sealer's time is " + coldDrinkManufacturing.currentTime);
            
            if (processingBottle != null && processingBottle.deliveryTime <= coldDrinkManufacturing.currentTime) {
                bottleReady = true;
            }

            if (bottleReady) {
                boolean bottleDelivered;
                if (processingBottle.state == 1) {
                    // put in Packager's tray
                    bottleDelivered = processingUnitReference.storeBottle(processingBottle.type);
                }
                else {
                    // add in sealing unit checking needs to be done
                    bottleDelivered = coldDrinkManufacturing.godown.storeBottle(processingBottle.type);
                }
                bottleReady = false;
                processingBottle = null;
                assert(bottleDelivered == true);
            }

            if (processingBottle == null) {
                if (isTrayEmpty()) { // take from unfinished tray
                    int newBottleType = coldDrinkManufacturing.unfinishedTray.takeBottle(bottleTypeUnfinished);
                    if (newBottleType != -1) {
                        processingBottle = new Bottle(newBottleType, 1, coldDrinkManufacturing.currentTime + 3); // first argument represnt type change accordingly
                        bottleTypeUnfinished = (newBottleType + 1) % 2;
                        System.out.println("Global se sealer ne liya");
                    }
                    else {
                        // simulation finished
                    }
                }
                else { // extract from Sealer's Tray
                    int newBottleType = takeBottle(bottleTypeTray);
                    processingBottle = new Bottle(newBottleType, 0, coldDrinkManufacturing.currentTime + 3); // first argument represnt type change accordingly
                    bottleTypeTray = (newBottleType + 1) % 2;
                    System.out.println("Tray se sealer ne liya");
                }
            }
            
            try { 
                coldDrinkManufacturing.sealingBarrier.await();
            }  
            catch (InterruptedException | BrokenBarrierException e) { 
                // e.printStackTrace(); 
            } 

            try { 
                // System.out.println("Sealing awaiting time");
                coldDrinkManufacturing.timeBarrier.await();
            }  
            catch (InterruptedException | BrokenBarrierException e) { 
                // e.printStackTrace(); 
            } 
        }
    }
}