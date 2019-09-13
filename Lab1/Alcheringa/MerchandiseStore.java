import java.util.*;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;

// Class to model a merchadise store
public class MerchandiseStore {
    private Inventory inventory;
    // Following data items store the input order list
    private int totalOrders;
    private Order[] orderList;
    // orderNumberSet to ensure order Id's are unique
    private Set<Integer> orderNumberSet;
    private Scanner inputScanner;

    // Initialise the store by initialising inventory and take order list
    public MerchandiseStore() {
        inputScanner = new Scanner(System.in);
        
        // 1. Take input the initial stock of each item type and put them in inventory 
        int initialStockS, initialStockM, initialStockL, initialStockC;
        System.out.print("Enter number of S shirts: ");
        initialStockS = inputScanner.nextInt();
        System.out.print("Enter number of M shirts: ");
        initialStockM = inputScanner.nextInt();
        System.out.print("Enter number of L shirts: ");
        initialStockL = inputScanner.nextInt();
        System.out.print("Enter number of Caps: ");
        initialStockC = inputScanner.nextInt();

        inventory = new Inventory(initialStockS, initialStockM, initialStockL, initialStockC);

        // 2. Input total number of orders to initialse order array to that size
        System.out.print("Enter total number of orders: ");
        totalOrders = inputScanner.nextInt();        
        orderList = new Order[totalOrders];
        orderNumberSet = new HashSet<Integer>();
        
        // Print initial stock
        System.out.println(inventory.getOverallStock());
    }

    // Validator function for rule out invalid orders with two-fold criteria
    private boolean validate(Order order) {
        // Order number spuulied should be unique
        if (orderNumberSet.contains(order.getOrderNumber())) {
            System.out.println("This order number has already been given");
            return false;
        }
        // Item type must be one of S M L C
        char itemType = order.getItemType();
        if (!(itemType == 'C' || itemType == 'S' || itemType == 'M' || itemType == 'L')) {
            System.out.println("Inventory does not support item of type " + itemType);
            return false;
        }
        return true;
    }

    // Take all the orders and put them in order list, rejecting invalid orders
    public void takeOrders() {
        System.out.println("Enter orders in format \"orderNumber itemType requiredUnits\":");
        int orderNumber, requiredUnits;
        char itemType;

        int currentOrder = 0;
        while (currentOrder < totalOrders) {
            orderNumber = inputScanner.nextInt();
            itemType = inputScanner.next().charAt(0);
            requiredUnits = inputScanner.nextInt();
            // Initialise an Order object with given parameters
            orderList[currentOrder] = new Order(orderNumber, itemType, requiredUnits);
            if (validate(orderList[currentOrder])) {
                orderNumberSet.add(orderNumber);
                currentOrder++;
            }
            // We are not incrementing currentOrder taken if last order is invalid
        }
        inputScanner.close();
    }

    // Service an order, the order will fail incase of insufficient stock 
    public void serviceOrders() {
        /* Creating an executor service to service multiple concurrent
        threads (at max equal to pool size here equal to 4) */
        ExecutorService threadPool = Executors.newFixedThreadPool(4);
        
        /* Iterating through the list of orders ,creating a thread
        for each order and adding it to executor service threadPool*/
        for (int i = 0; i < totalOrders; i++) {
            Runnable orderProcessor = new OrderProcessor(orderList[i], inventory);
            threadPool.execute(orderProcessor);
        }
        // Executor will ensure only 4 threads are active concurrently
        threadPool.shutdown();
    }

    public static void main(String[] args) {
        // Create a instance of the store, let it take and service orders
        MerchandiseStore alcheringaStore = new MerchandiseStore();
        alcheringaStore.takeOrders();
        alcheringaStore.serviceOrders();
    }
}

// Thread class to process a single order
class OrderProcessor implements Runnable {
    private Order order;
    private Inventory inventoryRef;

    public OrderProcessor(Order currentOrder, Inventory innventoryRef) {
        this.order = currentOrder;
        this.inventoryRef = innventoryRef;
    }

    public void run() {
        // invoke the inventory function which process
        // the recieved order (sychronised inside)
        inventoryRef.processOrder(order);
    }
}
