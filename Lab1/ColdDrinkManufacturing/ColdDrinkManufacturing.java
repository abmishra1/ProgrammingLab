import java.util.*;
import java.util.concurrent.CyclicBarrier; 
import java.util.concurrent.BrokenBarrierException; 

public class ColdDrinkManufacturing implements Runnable {
    public int currentTime;
    public int stopTime;
    public CyclicBarrier sealingBarrier; 
    public CyclicBarrier timeBarrier; 
    
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

        packagingUnit.sealingUnit = sealingUnit;
        sealingUnit.packagingUnit = packagingUnit;

        sealingBarrier = new CyclicBarrier(2); 
        timeBarrier = new CyclicBarrier(3, new Runnable() {
            public void run() {
                currentTime++;
                // System.out.println("Time is " + currentTime);
                // packagingUnit.tray.printTray();
                // sealingUnit.tray.printTray();
                // System.out.println("***************************");                
                if (currentTime <= stopTime) {
                    timeBarrier.reset();
                }
                // else { // Do not go gentle into that cold dark night
                    System.out.println("B1 packaged: " + packagingUnit.packagedBottle1Count);
                    System.out.println("B1 sealed: " + sealingUnit.sealedBottle1Count);
                    System.out.println("B1 in godown: " + godown.getBottleCount(1));
                    System.out.println("B2 packaged: " + packagingUnit.packagedBottle2Count);
                    System.out.println("B2 sealed: " + sealingUnit.sealedBottle2Count);
                    System.out.println("B2 in godown: " + godown.getBottleCount(0));
                    System.out.println("");
                // }
            }
        }); 
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
        t1.start();
        t2.start();
        
        while (this.currentTime <= this.stopTime) {
            try { 
                // System.out.println("Manufacturing Unit awaiting time");
                timeBarrier.await();
            }  
            catch (InterruptedException | BrokenBarrierException e) { 
                // e.printStackTrace(); 
            } 
        }
    }
}