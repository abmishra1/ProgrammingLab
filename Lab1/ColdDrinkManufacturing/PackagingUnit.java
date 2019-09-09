import java.util.concurrent.BrokenBarrierException; 

public class PackagingUnit implements Runnable{
    public PackagerTray tray;
    public boolean bottleReady;
    public int bottleTypeUnfinished;
    public int bottleTypeTray;
    public Bottle processingBottle;
    public SealingUnit sealingUnit;
    public ColdDrinkManufacturing coldDrinkManufacturing;

    public int packagedBottle1Count;
    public int packagedBottle2Count;

    public PackagingUnit(ColdDrinkManufacturing newColdDrinkManufacturing){
        tray = new PackagerTray(0, 0);
        bottleReady = false;
        bottleTypeUnfinished = 1;
        bottleTypeTray = 1;
        processingBottle = null;
        packagedBottle1Count = 0;
        packagedBottle1Count = 0;
        coldDrinkManufacturing = newColdDrinkManufacturing;
    }

    private void incrementPackagedBottleCount(int bottleType) {
        if (bottleType == 1) {
            packagedBottle1Count++;
        }
        else {
            packagedBottle2Count++;
        }
        return;
    }

    public void run() {
        while (coldDrinkManufacturing.currentTime <= coldDrinkManufacturing.stopTime) {
            // System.out.println("Packager's time is " + coldDrinkManufacturing.currentTime);
            if (processingBottle != null && processingBottle.deliveryTime <= coldDrinkManufacturing.currentTime) {
                bottleReady = true;
                if (processingBottle.deliveryTime == coldDrinkManufacturing.currentTime) {
                    incrementPackagedBottleCount(processingBottle.type);
                }
            }
    
            if (bottleReady) {
                boolean bottleDelivered; 
                if (processingBottle.state == 1) {
                    // attempt to put in Sealer Tray
                    // Determine exactly when Sealer will not pick up a new bottle
                    // commit to putting only if you know Sealer will take bottle in this stage
                    while (true) {
                        sealingUnit.tray.acquireLock();
                        bottleDelivered = sealingUnit.tray.storeBottle(processingBottle.type);
                        int nextDelivery = sealingUnit.tray.getNextDelivery();
                        sealingUnit.tray.releaseLock();
                        if (bottleDelivered || (nextDelivery > coldDrinkManufacturing.currentTime)) break; 
                    }
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
                tray.acquireLock();
                int newBottleType = tray.takeBottle(bottleTypeTray);
                tray.releaseLock();

                if (newBottleType == -1) {
                    newBottleType = coldDrinkManufacturing.unfinishedTray.takeBottle(bottleTypeUnfinished);
                    if (newBottleType != -1) {
                        processingBottle = new Bottle(newBottleType, 1, coldDrinkManufacturing.currentTime + 2); // first argument represnt type change accordingly
                        bottleTypeUnfinished = (newBottleType + 1) % 2;
                        // System.out.println("Global se packager utha rha");
                    }
                    // else simulation finished;
                }
                else {
                    processingBottle = new Bottle(newBottleType, 0, coldDrinkManufacturing.currentTime + 2); // first argument represnt type change accordingly
                    bottleTypeTray = (newBottleType + 1) % 2;
                    // System.out.println("Local tray se packager utha rha");
                }
            }
            
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