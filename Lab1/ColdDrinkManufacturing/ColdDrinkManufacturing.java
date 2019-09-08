import java.util.*;
import java.time.Clock;
import java.util.concurrent.CyclicBarrier; 

public class ColdDrinkManufacturing implements Runnable {
    public int currentTime;
    public int stopTime;
    public CyclicBarrier sealingBarrier; 
    public CyclicBarrier timeBarrier; 
    
    ProcessingUnit processingUnit;
    SealingUnit sealingUnit;
    Godown godown;
    UnfinishedTray unfinishedTray;
      
    public ColdDrinkManufacturing(int bottle1Count, int bottle2Count, int inputStopTime){
        currentTime = 0;
        stopTime = inputStopTime;
        unfinishedTray = new UnfinishedTray(bottle1Count, bottle2Count);
        godown = new Godown();
        processingUnit = new ProcessingUnit(this);
        sealingUnit = new SealingUnit(this);

        processingUnit.sealingUnitReference = sealingUnit;
        sealingUnit.processingUnitReference = processingUnit;

        sealingBarrier = new CyclicBarrier(2); 
        timeBarrier = new CyclicBarrier(2, new Runnable() {
            public void run() {
                currentTime++;
                System.out.println("***************************");
                System.out.println("Time is " + currentTime);
                processingUnit.printTray();
                sealingUnit.printTray();
                if (currentTime <= stopTime) {
                    timeBarrier.reset();
                }
            }
        }); 
    }

    public static void main(String[] args) 
    { 
        Scanner inputScanner = new Scanner(System.in);
        System.out.println("Enter number of Bottle1:");
        int numberOfBottle1 = inputScanner.nextInt();
        System.out.println("Enter number of Bottle2:");
        int numberOfBottle2 = inputScanner.nextInt();
        System.out.println("Enter Stop time:");
        int stopTime = inputScanner.nextInt();
            
        ColdDrinkManufacturing test = new ColdDrinkManufacturing(numberOfBottle1, numberOfBottle2, stopTime);
        Thread t1 = new Thread(test); 
        t1.start(); 
    } 

    public void run() {
        Thread t1 = new Thread(this.processingUnit);
        Thread t2 = new Thread(this.sealingUnit);
        t1.start();
        t2.start();
        
        // t1.join();
        // t2.join();
        // print stats
        // while (this.currentTime <= this.stopTime) {

        // }
        try {
            Thread.sleep(2000);
        } catch(InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println("B1 in godown: " + godown.getBottleCount(1));
        System.out.println("B2 in godown: " + godown.getBottleCount(0));
    }
}