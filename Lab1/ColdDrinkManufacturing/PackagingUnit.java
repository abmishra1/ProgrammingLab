public class PackagingUnit implements Runnable{
    public PackagerTray tray;
    private boolean bottleReady;
    private int bottleTypeUnfinished;
    private int bottleTypeTray;
    private Bottle processingBottle;
    
    public SealingUnit sealingUnit;
    private ColdDrinkManufacturing coldDrinkManufacturing;

    private int packagedBottle1Count;
    private int packagedBottle2Count;
    private int lastPackagedTime;

    public PackagingUnit(ColdDrinkManufacturing newColdDrinkManufacturing){
        tray = new PackagerTray(0, 0);
        bottleReady = false;
        processingBottle = null;
        bottleTypeUnfinished = 1;
        bottleTypeTray = 1;
        packagedBottle1Count = 0;
        packagedBottle1Count = 0;
        lastPackagedTime = 0;
        coldDrinkManufacturing = newColdDrinkManufacturing;
    }

    private void incrementPackagedBottleCount(int bottleType) {
        if (lastPackagedTime == coldDrinkManufacturing.packagerTime) return;
        if (bottleType == 1) {
            packagedBottle1Count++;
        }
        else {
            packagedBottle2Count++;
        }
        lastPackagedTime = coldDrinkManufacturing.packagerTime;
        return;
    }

    public void run() {
        // System.out.println("Packager's time is " + coldDrinkManufacturing.packagerTime);
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
                bottleDelivered = sealingUnit.tray.storeBottle(processingBottle.type);
                if (!bottleDelivered) {
                    coldDrinkManufacturing.packagerTime = coldDrinkManufacturing.sealerTime;
                    return;
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
            int newBottleType = tray.takeBottle(bottleTypeTray);

            if (newBottleType == -1) {
                newBottleType = coldDrinkManufacturing.unfinishedTray.takeBottle(bottleTypeUnfinished);
                if (newBottleType != -1) {
                    // first argument represnt type change accordingly
                    processingBottle = new Bottle(newBottleType, 1, coldDrinkManufacturing.currentTime + 2);
                    coldDrinkManufacturing.packagerTime += 2; 
                    bottleTypeUnfinished = (newBottleType + 1) % 2;
                    // System.out.println("Global se packager utha rha");
                }
                else {
                    coldDrinkManufacturing.packagerTime += 1;
                }
            }
            else {
                processingBottle = new Bottle(newBottleType, 0, coldDrinkManufacturing.currentTime + 2);
                coldDrinkManufacturing.packagerTime += 2;
                bottleTypeTray = (newBottleType + 1) % 2;
                // System.out.println("Local tray se packager utha rha");
            }
        }     
    }

    public int getBottleCount(int bottleType) {
        if (bottleType == 1) {
            return packagedBottle1Count;
        }
        else {
            return packagedBottle2Count;
        }
    }
}