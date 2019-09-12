public class SealingUnit implements Runnable {
    public SealerTray tray;
    private boolean bottleReady;
    private int bottleTypeUnfinished;
    private Bottle processingBottle;
    public PackagingUnit packagingUnit;
    private ColdDrinkManufacturing coldDrinkManufacturing;

    private int sealedBottle1Count;
    private int sealedBottle2Count;

    public SealingUnit(ColdDrinkManufacturing newColdDrinkManufacturing) {
        tray = new SealerTray(0, 0);
        bottleReady = false;
        processingBottle = null;
        bottleTypeUnfinished = 0;
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
        // System.out.println("Sealer's time is " + coldDrinkManufacturing.sealerTime);
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
                bottleDelivered = packagingUnit.tray.storeBottle(processingBottle.type);
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
            int newBottleType = tray.takeBottle();

            if (newBottleType == -1) {
                if (coldDrinkManufacturing.packagerTime == coldDrinkManufacturing.sealerTime) {
                    return;
                }
                newBottleType = coldDrinkManufacturing.unfinishedTray.takeBottle(bottleTypeUnfinished);
                if (newBottleType != -1) {
                    processingBottle = new Bottle(newBottleType, 1, coldDrinkManufacturing.currentTime + 3);
                    coldDrinkManufacturing.sealerTime += 3;
                    bottleTypeUnfinished = (newBottleType + 1) % 2;
                    // System.out.println("Global se sealer ne liya");
                }
                else {
                    coldDrinkManufacturing.sealerTime++;
                }
            }
            else {
                processingBottle = new Bottle(newBottleType, 0, coldDrinkManufacturing.currentTime + 3);
                coldDrinkManufacturing.sealerTime += 3;
                // System.out.println("Tray se sealer ne liya");
            }
        }
    }

    public int getBottleCount(int bottleType) {
        if (bottleType == 1) {
            return sealedBottle1Count;
        }
        else {
            return sealedBottle2Count;
        }
    }
}