import java.util.*;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;

class OrderProcessor implements Runnable {
    private Order order;
    private Inventory inventoryRef;

    public OrderProcessor(Order currentOrder, Inventory innventoryRef) {
        this.order = currentOrder;
        this.inventoryRef = innventoryRef;
    }

    public void run() {
        inventoryRef.processOrder(order);
    }
}

public class MerchandiseStore {
    private Inventory inventory;

    private int totalOrders;
    private Order[] orderList;
    private Set<Integer> orderNumberSet;

    private Scanner inputScanner;

    public MerchandiseStore() {
        inputScanner = new Scanner(System.in);

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

        System.out.print("Enter total number of orders: ");
        totalOrders = inputScanner.nextInt();        
        orderList = new Order[totalOrders];
        orderNumberSet = new HashSet<Integer>();
        
        System.out.println(inventory.getOverallStock());
    }

    private boolean validate(Order order) {
        if (orderNumberSet.contains(order.getOrderNumber())) {
            System.out.println("This order number has already been given");
            return false;
        }
        char itemType = order.getItemType();
        if (!(itemType == 'C' || itemType == 'S' || itemType == 'M' || itemType == 'L')) {
            System.out.println("Inventory does not support item of type " + itemType);
            return false;
        }
        return true;
    }

    public void takeOrders() {
        System.out.println("Enter orders in format \"orderNumber itemType requiredUnits\":");
        int orderNumber, requiredUnits;
        char itemType;

        int currentOrder = 0;
        while (currentOrder < totalOrders) {
            orderNumber = inputScanner.nextInt();
            itemType = inputScanner.next().charAt(0);
            requiredUnits = inputScanner.nextInt();
            orderList[currentOrder] = new Order(orderNumber, itemType, requiredUnits);
            if (validate(orderList[currentOrder])) {
                orderNumberSet.add(orderNumber);
                currentOrder++;
            }
        }
        inputScanner.close();
    }

    public void serviceOrders() {
        ExecutorService threadPool = Executors.newFixedThreadPool(4);
        for (int i = 0; i < totalOrders; i++) {
            Runnable orderProcessor = new OrderProcessor(orderList[i], inventory);
            threadPool.execute(orderProcessor);
        }
        threadPool.shutdown();
    }

    public static void main(String[] args) {
        MerchandiseStore alcheringaStore = new MerchandiseStore();
        alcheringaStore.takeOrders();
        alcheringaStore.serviceOrders();
    }
}