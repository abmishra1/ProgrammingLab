public class InventoryItem {
    private int currentStock;
    private Inventory inventory;

    public InventoryItem(int initialStock, Inventory inventory) {
        this.currentStock = initialStock;
        this.inventory = inventory;
    }

    public int getCurrentStock() {
        return currentStock;
    }

    public synchronized void processOrder(Order order) {
        int requiredUnits = order.getRequiredUnits();
        int orderIndex = order.getOrderNumber();
        if (requiredUnits > currentStock) {
            String overallStock = inventory.getOverallStock();        
            System.out.println("Order " + orderIndex + " failed.    " + overallStock);
            return;
        }
        currentStock = currentStock - requiredUnits;
        String overallStock = inventory.getOverallStock();
        System.out.println("Order " + orderIndex + " successful. " + overallStock);
    }
}
