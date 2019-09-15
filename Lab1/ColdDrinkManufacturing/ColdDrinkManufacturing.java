/* 
    Authors' Name : Abhinav Mishra, Nitin Kedia
*/
// import libraries
import java.util.*;

// Class represents an instance of Cold Drink Manufacturing Factory
public class ColdDrinkManufacturing implements Runnable {
    // variable to store current time
    public int currentTime;
    // variable to store packager time
    public int packagerTime;
    // variable to store sealer time 
    public int sealerTime;
    // variable to store stop time
    public int stopTime;
    
    // Variable to store objects of different units of the factory
    PackagingUnit packagingUnit;
    SealingUnit sealingUnit;
    Godown godown;
    UnfinishedTray unfinishedTray;
      
    // Constructor to initialize the member variables
    public ColdDrinkManufacturing(int bottle1Count, int bottle2Count, int inputStopTime){
        currentTime = 0;
        stopTime = inputStopTime;
        unfinishedTray = new UnfinishedTray(bottle1Count, bottle2Count);
        godown = new Godown();
        packagingUnit = new PackagingUnit(this);
        sealingUnit = new SealingUnit(this);
        packagerTime = 0;
        sealerTime = 0;

        packagingUnit.sealingUnit = sealingUnit;
        sealingUnit.packagingUnit = packagingUnit;
    }

    // Main function of program execution starts here
    public static void main(String[] args) 
    { 
        // create a input scanner and take input the number of B1,B2 ad stop time
        Scanner inputScanner = new Scanner(System.in);
        System.out.print("Enter initial number of B1: ");
        int numberOfBottle1 = inputScanner.nextInt();
        System.out.print("Enter initial number of B2: ");
        int numberOfBottle2 = inputScanner.nextInt();
        System.out.print("Enter stop time: ");
        int stopTime = inputScanner.nextInt();
        inputScanner.close();   

        // create a new object of the ColdDrinkManufacturing class
        ColdDrinkManufacturing coldDrinkManufacturingFactory = new ColdDrinkManufacturing(numberOfBottle1, numberOfBottle2, stopTime);
        // create a new thread for the object and start its execution
        Thread packagerThread = new Thread(coldDrinkManufacturingFactory); 
        packagerThread.start(); 
    } 

    // function to print the final statistics of the factory
    public void printStatistics() {
        System.out.println("B1 packaged:  " + packagingUnit.getBottleCount(1));
        System.out.println("B1 sealed:    " + sealingUnit.getBottleCount(1));
        System.out.println("B1 in godown: " + godown.getBottleCount(1));
        System.out.println("B2 packaged:  " + packagingUnit.getBottleCount(0));
        System.out.println("B2 sealed:    " + sealingUnit.getBottleCount(0));
        System.out.println("B2 in godown: " + godown.getBottleCount(0));
    }

    // Function the thread object created runs
    public void run() {
        // create two thread for the packaging and sealing unit
        Thread packagerThread = new Thread(this.packagingUnit);
        Thread sealerThread = new Thread(this.sealingUnit);
        
        // packagerThread and sealerThread post their next run time
        try {
            // Current time is calculated as the minimun time of both units
            currentTime = Math.min(packagerTime, sealerTime);
            // if current time is less than equal to sealing unit it means execution is still reamining
            while (currentTime <= stopTime) {
                // if both units are at same time as current create thread to run both units
                if (packagerTime == currentTime && sealerTime == currentTime) {
                    packagerThread = new Thread(this.packagingUnit);
                    sealerThread = new Thread(this.sealingUnit);
                    packagerThread.start();
                    sealerThread.start();
                    packagerThread.join();
                    sealerThread.join();
                }
                // if only packager unit time is equal to current , we must run packaging unit to bring it upto sealer
                else if (packagerTime == currentTime) {
                    packagerThread = new Thread(this.packagingUnit);
                    packagerThread.start();
                    packagerThread.join();
                }
                // if only sealing unit time is equal to current , we must run sealing unit to bring it upto packager
                else if (sealerTime == currentTime) {
                    sealerThread = new Thread(this.sealingUnit);
                    sealerThread.start();
                    sealerThread.join();
                }
                // re calculate current time
                currentTime = Math.min(packagerTime, sealerTime);
            }
        }
        catch (InterruptedException e){
            e.printStackTrace();
        }
        printStatistics();
    }
}