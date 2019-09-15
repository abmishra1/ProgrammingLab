/* 
    Authors' Name : Abhinav Mishra, Nitin Kedia
    Description: Class representing the inventory of the store 
*/
public class Inventory {
    // House the 4 item types
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

    // Function to return printable string of the overall stock in the inventory
    public String getOverallStock() {
        String overallStock = "Inventory ";
        overallStock += ("S: " + smallShirts.getCurrentStock()) + " ";
        overallStock += ("M: " + mediumShirts.getCurrentStock()) + " ";
        overallStock += ("L: " + largeShirts.getCurrentStock()) + " ";
        overallStock += ("C: " + caps.getCurrentStock()) + " ";
        return overallStock;
    }

    // Function to segragate orders by item type and
    // send to corresponding sub-inventory to actually process
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