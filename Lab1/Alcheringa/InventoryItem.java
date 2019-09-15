/* 
    Author's Name : Abhinav Mishra, Nitin Kedia
    Desription: Class for managing one type of item in the inventory
*/
public class InventoryItem {
    private int currentStock;
    private Inventory inventory; // parent reference

    public InventoryItem(int initialStock, Inventory inventory) {
        this.currentStock = initialStock;
        this.inventory = inventory;
    }

    public int getCurrentStock() {
        return currentStock;
    }

    /* Following function processes any order for the inventory item by
    checking if the demand is less than or equal to the current stock,
    if it is so the order is successful the current stock value is updated
    else order failure is notified.
    The synchronized keyword is used to allow only one thread to access it
    at any particular time */
    public synchronized void processOrder(Order order) {
        int requiredUnits = order.getRequiredUnits();
        int orderIndex = order.getOrderNumber();
        // Checking if order can be serviced or not
        if (requiredUnits > currentStock) {
            String overallStock = inventory.getOverallStock();        
            System.out.println("Order " + orderIndex + " failed.    " + overallStock);
            return;
        }
        // Updating the current stock value.
        currentStock = currentStock - requiredUnits;
        // Get overall stock from parent (whole inventory)
        String overallStock = inventory.getOverallStock();
        System.out.println("Order " + orderIndex + " successful. " + overallStock);
    }
}
