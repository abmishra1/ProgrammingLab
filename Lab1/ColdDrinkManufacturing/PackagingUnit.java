/* 
    Authors' Name : Abhinav Mishra, Nitin Kedia
*/
// class to represent a packaging unit
public class PackagingUnit implements Runnable{
    // packager tray variable
    public PackagerTray tray;
    // varible to store if a bottle is sealed or not
    private boolean bottleReady;
    // variable to store which bottle type was last taken from the unfinished tray
    private int bottleTypeUnfinished;
    // variable to store which type of bottle needs to be taken next time from packaging tray
    private int bottleTypeTray;
    // variable to store the bottle which is being processed for packaging
    private Bottle processingBottle;
    // variable to store reference to the sealing unit object
    public SealingUnit sealingUnit;
    // variable to store reference to the main Manufacturing factory object
    private ColdDrinkManufacturing coldDrinkManufacturing;
    // variables to store count of packaged bottle types
    private int packagedBottle1Count;
    private int packagedBottle2Count;
    // variable to store last packaged time
    private int lastPackagedTime;

    // constructor to initiaize member variables
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

    // function to increment the number of packaged bottles of a particular type
    private void incrementPackagedBottleCount(int bottleType) {
        if (lastPackagedTime == coldDrinkManufacturing.packagerTime) return;
        // if bottle type is 1 increment count of packaged bottle 1
        if (bottleType == 1) {
            packagedBottle1Count++;
        }
         // else increment type of packaged bottle 2
        else {
            packagedBottle2Count++;
        }
        // set last packaged time
        lastPackagedTime = coldDrinkManufacturing.packagerTime;
        return;
    }

    // function called when the thread of this object is executed
    public void run() {
        // if there is a processing bottle and its delivery time has been reached it means we have a bottle ready
        if (processingBottle != null && processingBottle.deliveryTime <= coldDrinkManufacturing.currentTime) {
            bottleReady = true;
            // increment total packaged bottle type count when current time equals delivery time of processing bottle
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
                // if bottle delivered reset the bottleReady and processing Bottle
                bottleReady = false;
                processingBottle = null;
            }
        }
            
        // if processing bottle is null we must extract one bottle from the trays
        if (processingBottle == null) {
            // attempt to take from sealer tray, if false then go to unfinished
            int newBottleType = tray.takeBottle(bottleTypeTray);
            // if -1 is returned it means tray is empty take from unfinished tray requesting the alternate botttle from last time
            if (newBottleType == -1) {
                newBottleType = coldDrinkManufacturing.unfinishedTray.takeBottle(bottleTypeUnfinished);
                // if returned bottle type is not -1 it means we have a valid bottle which is neither packaged or sealed  i.e state 1
                if (newBottleType != -1) {
                    // first argument represnt type change accordingly
                    processingBottle = new Bottle(newBottleType, 1, coldDrinkManufacturing.currentTime + 2);
                    coldDrinkManufacturing.packagerTime += 2; 
                     /* set bottleTypeUnfinished denoting next time which bottle will be requested 
                    from unfinished tray to be alternate of received one */
                    bottleTypeUnfinished = (newBottleType + 1) % 2;
                    // System.out.println("Global se packager utha rha");
                }
                else {
                    // increase sealer time
                    coldDrinkManufacturing.packagerTime += 1;
                }
            }
            // if -1 is not returned we have a packaged bottle so a new processing bottle will be created with state 0
            else {
                // create a new processing bottle of this bottle type and set the delivery time to current time + 2
                processingBottle = new Bottle(newBottleType, 0, coldDrinkManufacturing.currentTime + 2);
                 // set the next time sealer will be active to current time + 2
                coldDrinkManufacturing.packagerTime += 2;
                // alternate the last bottle type from tray
                bottleTypeTray = (newBottleType + 1) % 2;
            }
        }     
    }

    // Function to return number of sealed bottles of a particular bottle type
    public int getBottleCount(int bottleType) {
        // if bottle type is 1 return sealed bottle 1 count
        if (bottleType == 1) {
            return packagedBottle1Count;
        }
        //  return sealed bottle 2 count
        else {
            return packagedBottle2Count;
        }
    }
}