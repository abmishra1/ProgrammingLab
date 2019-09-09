import java.util.*;

public class ColdDrinkManufacturing implements Runnable {
    public int currentTime;
    public int runTime1;
    public int runTime2;
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
        runTime1 = 0;
        runTime2 = 0;

        packagingUnit.sealingUnit = sealingUnit;
        sealingUnit.packagingUnit = packagingUnit;
    }

    public static void main(String[] args) 
    { 
        Scanner inputScanner = new Scanner(System.in);
        // System.out.println("Enter number of Bottle1:");
        int numberOfBottle1 = inputScanner.nextInt();
        // System.out.println("Enter number of Bottle2:");
        int numberOfBottle2 = inputScanner.nextInt();
        // System.out.println("Enter Stop time:");
        int stopTime = inputScanner.nextInt();
        inputScanner.close();   

        ColdDrinkManufacturing test = new ColdDrinkManufacturing(numberOfBottle1, numberOfBottle2, stopTime);
        Thread t1 = new Thread(test); 
        t1.start(); 
    } 

    public void run() {
        Thread t1 = new Thread(this.packagingUnit);
        Thread t2 = new Thread(this.sealingUnit);
        
        // t1 and t2 post their next run time
        try {
            currentTime = Math.min(runTime1, runTime2);
            while (currentTime <= stopTime) {
                // System.out.println("Current time is: " + currentTime);
                if (runTime1 == currentTime && runTime2 == currentTime) {
                    t1 = new Thread(this.packagingUnit);
                    t2 = new Thread(this.sealingUnit);
                    t1.start();
                    t2.start();
                    t1.join();
                    t2.join();
                }
                else if (runTime1 == currentTime) {
                    t1 = new Thread(this.packagingUnit);
                    t1.start();
                    t1.join();
                }
                else if (runTime2 == currentTime) {
                    t2 = new Thread(this.sealingUnit);
                    t2.start();
                    t2.join();
                }
                currentTime = Math.min(runTime1, runTime2);
            }
        }
        catch (InterruptedException e){
            e.printStackTrace();
        }
        System.out.println("B1 packaged: " + packagingUnit.packagedBottle1Count);
        System.out.println("B1 sealed: " + sealingUnit.sealedBottle1Count);
        System.out.println("B1 in godown: " + godown.getBottleCount(1));
        System.out.println("B2 packaged: " + packagingUnit.packagedBottle2Count);
        System.out.println("B2 sealed: " + sealingUnit.sealedBottle2Count);
        System.out.println("B2 in godown: " + godown.getBottleCount(0));
        System.out.println("");
    }
}