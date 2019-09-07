import java.util.*;

public class Inventory {
    static InventoryItem smallShirts;
    static InventoryItem mediumShirts;
    static InventoryItem largeShirts;
    static InventoryItem caps;

    public Inventory(Scanner inputScanner) {
        int initialStock;
        System.out.println("Enter number of S shirts:");
        initialStock = inputScanner.nextInt();
        smallShirts = new InventoryItem(initialStock);

        System.out.println("Enter number of M shirts:");
        initialStock = inputScanner.nextInt();
        mediumShirts = new InventoryItem(initialStock);
        
        System.out.println("Enter number of L shirts:");
        initialStock = inputScanner.nextInt();
        largeShirts = new InventoryItem(initialStock);
        
        System.out.println("Enter number of Caps:");
        initialStock = inputScanner.nextInt();
        caps = new InventoryItem(initialStock);
    }

    public static String getOverallStock() {
        String overallStock = "Inventory ";
        overallStock += ("S: " + smallShirts.getCurrentStock()) + " ";
        overallStock += ("M: " + mediumShirts.getCurrentStock()) + " ";
        overallStock += ("L: " + largeShirts.getCurrentStock()) + " ";
        overallStock += ("C: " + caps.getCurrentStock()) + " ";
        return overallStock;
    }

    public void processOrder(Order order) {
        switch (order.itemType) {
            case 'S':
                smallShirts.processOrder(order);
                break;
            case 'M':
                mediumShirts.processOrder(order);
                break;
            case 'L':
                largeShirts.processOrder(order);
                break;
            case 'C':
                caps.processOrder(order);
                break;
            default:
                break;
        }
    }
}