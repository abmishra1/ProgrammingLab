import java.util.*;

public class ColdDrinkManufacturing implements Runnable {
    public int currentTime;
    public int packagerTime;
    public int sealerTime;
    public int stopTime;
    
    PackagingUnit packagingUnit;
    SealingUnit sealingUnit;
    Godown godown;
    UnfinishedTray unfinishedTray;
      
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

    public static void main(String[] args) 
    { 
        Scanner inputScanner = new Scanner(System.in);
        System.out.print("Enter initial number of B1: ");
        int numberOfBottle1 = inputScanner.nextInt();
        System.out.print("Enter initial number of B2: ");
        int numberOfBottle2 = inputScanner.nextInt();
        System.out.print("Enter stop time: ");
        int stopTime = inputScanner.nextInt();
        inputScanner.close();   

        ColdDrinkManufacturing test = new ColdDrinkManufacturing(numberOfBottle1, numberOfBottle2, stopTime);
        Thread packagerThread = new Thread(test); 
        packagerThread.start(); 
    } 

    public void printStatistics() {
        System.out.println("B1 packaged:  " + packagingUnit.getBottleCount(1));
        System.out.println("B1 sealed:    " + sealingUnit.getBottleCount(1));
        System.out.println("B1 in godown: " + godown.getBottleCount(1));
        System.out.println("B2 packaged:  " + packagingUnit.getBottleCount(0));
        System.out.println("B2 sealed:    " + sealingUnit.getBottleCount(0));
        System.out.println("B2 in godown: " + godown.getBottleCount(0));
    }

    public void run() {
        Thread packagerThread = new Thread(this.packagingUnit);
        Thread sealerThread = new Thread(this.sealingUnit);
        
        // packagerThread and sealerThread post their next run time
        try {
            currentTime = Math.min(packagerTime, sealerTime);
            while (currentTime <= stopTime) {
                // System.out.println("Current time is: " + currentTime);
                if (packagerTime == currentTime && sealerTime == currentTime) {
                    packagerThread = new Thread(this.packagingUnit);
                    sealerThread = new Thread(this.sealingUnit);
                    packagerThread.start();
                    sealerThread.start();
                    packagerThread.join();
                    sealerThread.join();
                }
                else if (packagerTime == currentTime) {
                    packagerThread = new Thread(this.packagingUnit);
                    packagerThread.start();
                    packagerThread.join();
                }
                else if (sealerTime == currentTime) {
                    sealerThread = new Thread(this.sealingUnit);
                    sealerThread.start();
                    sealerThread.join();
                }
                currentTime = Math.min(packagerTime, sealerTime);
            }
        }
        catch (InterruptedException e){
            e.printStackTrace();
        }
        printStatistics();
    }
}