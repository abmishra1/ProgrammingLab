import java.util.*;

public class Inventory {
    private InventoryItem smallShirts;
    private InventoryItem mediumShirts;
    private InventoryItem largeShirts;
    private InventoryItem caps;

    public Inventory(int initialStockS, int initialStockM, int initialStockL, int initialStockC) {
        smallShirts = new InventoryItem(initialStockS, this);
        mediumShirts = new InventoryItem(initialStockM, this);
        largeShirts = new InventoryItem(initialStockL, this);
        caps = new InventoryItem(initialStockC, this);
    }

    public String getOverallStock() {
        String overallStock = "Inventory ";
        overallStock += ("S: " + smallShirts.getCurrentStock()) + " ";
        overallStock += ("M: " + mediumShirts.getCurrentStock()) + " ";
        overallStock += ("L: " + largeShirts.getCurrentStock()) + " ";
        overallStock += ("C: " + caps.getCurrentStock()) + " ";
        return overallStock;
    }

    public void processOrder(Order order) {
        switch (order.getItemType()) {
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