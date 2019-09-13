// class to represent a sealing unit
public class SealingUnit implements Runnable {
    // sealer tray variable
    public SealerTray tray;
    // varible to store if a bottle is sealed or not
    private boolean bottleReady;
    // variable to store which bottle type was last taken from the unfinished tray
    private int bottleTypeUnfinished;
    // variable to store the bottle which is being processed for sealing
    private Bottle processingBottle;
    // variable to store reference to the packaging unit object
    public PackagingUnit packagingUnit;
    // variable to store reference to the main Manufacturing factory object
    private ColdDrinkManufacturing coldDrinkManufacturing;
    // variables to store count of sealed bottle types
    private int sealedBottle1Count;
    private int sealedBottle2Count;

    // constructor to initialize member variables
    public SealingUnit(ColdDrinkManufacturing newColdDrinkManufacturing) {
        tray = new SealerTray();
        bottleReady = false;
        processingBottle = null;
        bottleTypeUnfinished = 0;
        sealedBottle1Count = 0;
        sealedBottle2Count = 0;
        coldDrinkManufacturing = newColdDrinkManufacturing;
    }

    // function to increment the number of sealed bottles of a particular type
    private void incrementSealedBottleCount(int bottleType) {
        // if bottle type is 1 increment count of sealed bottle 1
        if (bottleType == 1) {
            sealedBottle1Count++;
        }
        // else increment type of sealed bottle 2
        else {
            sealedBottle2Count++;
        }
        return;
    }

    // function called when the thread of this object is executed
    public void run() {
        // if there is a processing bottle and its delivery time has been reached it means we have a bottle ready
        if (processingBottle != null && processingBottle.deliveryTime <= coldDrinkManufacturing.currentTime) {
            bottleReady = true;
            // increment total sealed bottle type count when current time equals delivery time of processing bottle
            if (processingBottle.deliveryTime == coldDrinkManufacturing.currentTime) {
                incrementSealedBottleCount(processingBottle.type);
            }
        }

        // if there is already a ready bottle we must try to deliver it to the packaging unit or godown depending on its state
        if (bottleReady) {
            boolean bottleDelivered;
            if (processingBottle.state == 1) {
                // put in Packager's tray
                bottleDelivered = packagingUnit.tray.storeBottle(processingBottle.type);
            }
            else {
                // put it in godown
                bottleDelivered = coldDrinkManufacturing.godown.storeBottle(processingBottle.type);
            }
            // bottle delivered therefore change bottle ready to false and processing bottle to null
            bottleReady = false;
            processingBottle = null;
            assert(bottleDelivered == true);
        }
        
        // if processing bottle is null we must extract one bottle from the trays
        if (processingBottle == null) {
            // attempt to take from sealer tray, if false then go to unfinished
            int newBottleType = tray.takeBottle();
            // if -1 is returned it means tray is empty take from unfinished tray requesting the alternate botttle from last time
            if (newBottleType == -1) {
                if (coldDrinkManufacturing.packagerTime == coldDrinkManufacturing.sealerTime) {
                    return;
                }
                newBottleType = coldDrinkManufacturing.unfinishedTray.takeBottle(bottleTypeUnfinished);

                // if returned bottle type is not -1 it means we have a valid bottle which is neither packaged or sealed  i.e state 1
                if (newBottleType != -1) {
                    // create a new processing bottle of this bottle type and set the delivery time to current time + 3
                    processingBottle = new Bottle(newBottleType, 1, coldDrinkManufacturing.currentTime + 3);
                    // set the next time sealer will be active to current time + 3
                    coldDrinkManufacturing.sealerTime += 3;
                    /* set bottleTypeUnfinished denoting next time which bottle will be requested 
                    from unfinished tray to be alternate of received one */
                    bottleTypeUnfinished = (newBottleType + 1) % 2;
                }
                else {
                    // increase sealer time
                    coldDrinkManufacturing.sealerTime++;
                }
            }
            // if -1 is not returned we have a packaged bottle so a new processing bottle will be created with state 0
            else {
                // create a new processing bottle of this bottle type and set the delivery time to current time + 3
                processingBottle = new Bottle(newBottleType, 0, coldDrinkManufacturing.currentTime + 3);
                // set the next time sealer will be active to current time + 3
                coldDrinkManufacturing.sealerTime += 3;
            }
        }
    }

    // Function to return number of sealed bottles of a particular bottle type
    public int getBottleCount(int bottleType) {
        // if bottle type is 1 return sealed bottle 1 count
        if (bottleType == 1) {
            return sealedBottle1Count;
        }
        //  return sealed bottle 2 count
        else {
            return sealedBottle2Count;
        }
    }
}