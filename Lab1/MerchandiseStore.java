import java.util.*;
import java.util.concurrent.Executors; 
import java.util.concurrent.ExecutorService; 

class OrderProcessor implements Runnable {
    private Order order; 
    private Inventory inventory;

    public OrderProcessor(Order currentOrder, Inventory currentInventory) 
    { 
        order = currentOrder;
        inventory = currentInventory;
    } 

    public void run() 
    { 
        inventory.updateItemCount(order); 
    } 
}

public class MerchandiseStore {
    private static Scanner inputScanner;
    private static Inventory inventory;
    private static Order[] orderList;
    private static Set<Integer> orderNumberSet;
    
    private static boolean validate (Order order){
        if(orderNumberSet.contains(order.orderNumber)){
            System.out.println("This order number has already been given");
            return false;
        }
        char itemType = order.itemType;
        if (!(itemType == 'C' || itemType == 'S' || itemType == 'M' || itemType == 'L')){
            System.out.println("Inventory does not support item of type "+ itemType);
            return false;
        }
        return true;
    }
    public static void main(String[] args) {
        orderNumberSet = new HashSet<Integer>();
        inputScanner = new Scanner(System.in); 

        // Load Inventory
        System.out.println("Enter initial inventory in order S M L C:");
        int inventoryItemCountArray[] = new int[4];        
        for(int i = 0; i < 4; i++){
            inventoryItemCountArray[i] = inputScanner.nextInt();
        }
        inventory = new Inventory(inventoryItemCountArray);
        
        // Make order array
        System.out.println("Enter number of Orders:");
        int numberOfOrders = inputScanner.nextInt();
        orderList = new Order[numberOfOrders];
        
        System.out.println("Enter orders in format \"orderNumber itemType requiredUnits\"");
        int orderNumber, requiredUnits;
        char itemType;

        int currentOrder = 0;
        while (currentOrder < numberOfOrders) {
            orderNumber = inputScanner.nextInt(); 
            itemType = inputScanner.next().charAt(0);
            requiredUnits = inputScanner.nextInt();    
            orderList[currentOrder] = new Order(orderNumber, itemType, requiredUnits);
            if (validate(orderList[currentOrder])) {
                orderNumberSet.add(orderNumber);
                currentOrder++;
            }
        }

        ExecutorService threadPool = Executors.newFixedThreadPool(4);
        for(int i = 0; i < numberOfOrders; i++){
            Runnable orderProcessor = new OrderProcessor(orderList[i], inventory);
            threadPool.execute(orderProcessor);
        }        

        threadPool.shutdown();
    }
}