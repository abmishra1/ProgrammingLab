import java.util.concurrent.TimeoutException; 
import java.util.concurrent.BrokenBarrierException; 

public class ProcessingUnit implements Runnable{
    public Tray trays;
    public boolean bottleReady;
    public int bottleTypeUnfinished;
    public int bottleTypeTray;
    public Bottle processingBottle;
    public SealingUnit sealingUnitReference;
    public ColdDrinkManufacturing coldDrinkManufacturing;

    public ProcessingUnit(ColdDrinkManufacturing newColdDrinkManufacturing){
        trays = new Tray(0,0);
        bottleReady = false;
        bottleTypeUnfinished = 1;
        bottleTypeTray = 1;
        processingBottle = null;
        coldDrinkManufacturing = newColdDrinkManufacturing;
    }

    public boolean isTrayEmpty(){
        if (trays.bottle1 == 0 && trays.bottle2 == 0)
            return true;
        return false;
    }

    private boolean isBottleAvailable(int bottleType) {
        if (bottleType == 1) {
            return (trays.bottle1 > 0);
        }
        return (trays.bottle2 > 0);
    }

    private boolean isBottle1TrayFull(){
        return trays.bottle1>=2;
    }

    private boolean isBottle2TrayFull(){
        return trays.bottle2>=3;
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

    public boolean storeBottle(int bottleType) {
        if (bottleType == 1) {
            if(isBottle1TrayFull())
                return false;
            trays.bottle1++;
        }
        else {
            if(isBottle2TrayFull())
                return false;
            trays.bottle2++;
        }
        return true;
    }

    public void printTray() {
        System.out.println("B1 in Packager's tray: " + trays.bottle1);
        System.out.println("B2 in Packager's tray: " + trays.bottle2);
    }


    public void run() {
        while (coldDrinkManufacturing.currentTime <= coldDrinkManufacturing.stopTime) {
            // System.out.println("Packager's time is " + coldDrinkManufacturing.currentTime);

            if (processingBottle != null && processingBottle.deliveryTime <= coldDrinkManufacturing.currentTime){
                bottleReady = true;
            }
    
            try { 
                coldDrinkManufacturing.sealingBarrier.await();
            }  
            catch (InterruptedException | BrokenBarrierException e) { 
                // e.printStackTrace(); 
            } 

            if (bottleReady) {
                boolean bottleDelivered; 
                if (processingBottle.state == 1) {
                    // attempt to put in Sealer Tray
                    bottleDelivered = sealingUnitReference.storeBottle(processingBottle.type);
                }
                else { // put in Godown
                    bottleDelivered = coldDrinkManufacturing.godown.storeBottle(processingBottle.type);
                }
                if (bottleDelivered) {
                    bottleReady = false;
                    processingBottle = null;
                }
            }
                
            if (processingBottle == null) {
                if (isTrayEmpty()) { // attempt to extract from unfinished tray
                    System.out.println("Global tray se packager utha rha");
                    int newBottleType = coldDrinkManufacturing.unfinishedTray.takeBottle(bottleTypeUnfinished);
                    if (newBottleType != -1) {
                        processingBottle = new Bottle(newBottleType, 1, coldDrinkManufacturing.currentTime + 2); // first argument represnt type change accordingly
                        bottleTypeUnfinished = (newBottleType + 1) % 2;
                    }
                    else {
                        // simulation finished
                    }
                }
                else { // extract from Packager's Tray
                System.out.println("Local tray se packager utha rha");
                    int newBottleType = takeBottle(bottleTypeTray);
                    processingBottle = new Bottle(newBottleType, 0, coldDrinkManufacturing.currentTime + 2); // first argument represnt type change accordingly
                    bottleTypeTray = (newBottleType + 1) % 2;
                }
            }
            // increment global time

            
            try { 
                // System.out.println("Packaging awaiting time");
                coldDrinkManufacturing.timeBarrier.await();
            }  
            catch (InterruptedException | BrokenBarrierException e) { 
                // e.printStackTrace(); 
            } 
        }        
    }
}