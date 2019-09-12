public class SealingUnit implements Runnable {
    public SealerTray tray;
    public boolean bottleReady;
    public int bottleTypeUnfinished;
    public int bottleTypeTray;
    public Bottle processingBottle;
    public PackagingUnit packagingUnit;
    public ColdDrinkManufacturing coldDrinkManufacturing;

    public int sealedBottle1Count;
    public int sealedBottle2Count;

    public SealingUnit(ColdDrinkManufacturing newColdDrinkManufacturing) {
        tray = new SealerTray(0, 0);
        bottleReady = false;
        bottleTypeUnfinished = 0;
        bottleTypeTray = 1;
        processingBottle = null;
        sealedBottle1Count = 0;
        sealedBottle2Count = 0;
        coldDrinkManufacturing = newColdDrinkManufacturing;
    }

    private void incrementSealedBottleCount(int bottleType) {
        if (bottleType == 1) {
            sealedBottle1Count++;
        }
        else {
            sealedBottle2Count++;
        }
        return;
    }

    public void run() {
        // System.out.println("Sealer's time is " + coldDrinkManufacturing.runTime2);
        
        if (processingBottle != null && processingBottle.deliveryTime <= coldDrinkManufacturing.currentTime) {
            bottleReady = true;
            if (processingBottle.deliveryTime == coldDrinkManufacturing.currentTime) {
                incrementSealedBottleCount(processingBottle.type);
            }
        }

        if (bottleReady) {
            boolean bottleDelivered;
            if (processingBottle.state == 1) {
                // put in Packager's tray
                packagingUnit.tray.acquireLock();
                bottleDelivered = packagingUnit.tray.storeBottle(processingBottle.type);
                packagingUnit.tray.releaseLock();
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
            // attempt to take from sealer tray, if false then go to unfinished
            tray.acquireLock();
            int newBottleType = tray.takeBottle(bottleTypeTray);
            tray.releaseLock();

            
            if (newBottleType == -1) {
                if (coldDrinkManufacturing.runTime1 == coldDrinkManufacturing.runTime2) {
                    return;
                }
                newBottleType = coldDrinkManufacturing.unfinishedTray.takeBottle(bottleTypeUnfinished);
                if (newBottleType != -1) {
                    processingBottle = new Bottle(newBottleType, 1, coldDrinkManufacturing.currentTime + 3); // first argument represnt type change accordingly
                    coldDrinkManufacturing.runTime2 += 3;
                    bottleTypeUnfinished = (newBottleType + 1) % 2;
                    // System.out.println("Global se sealer ne liya");
                }
                else {
                    coldDrinkManufacturing.runTime2++;
                }
            }
            else {
                processingBottle = new Bottle(newBottleType, 0, coldDrinkManufacturing.currentTime + 3); // first argument represnt type change accordingly
                coldDrinkManufacturing.runTime2 += 3;
                // bottleTypeTray = (newBottleType + 1) % 2;
                // System.out.println("Tray se sealer ne liya");
            }
        }
    }
}