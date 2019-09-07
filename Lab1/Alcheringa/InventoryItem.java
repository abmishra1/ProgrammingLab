public class InventoryItem {
    private int currentStock;

    public InventoryItem(int initialStock) {
        currentStock = initialStock;
    }

    public int getCurrentStock() {
        return currentStock;
    }

    public synchronized void processOrder(Order order) {
        int requiredUnits = order.requiredUnits;
        int orderIndex = order.orderNumber;
        if (requiredUnits > currentStock) {
            System.out.println("Order " + orderIndex + " failed.");
            return;
        }
        currentStock = currentStock - requiredUnits;
        String overallStock = Inventory.getOverallStock();
        System.out.println("Order " + orderIndex + " successful. " + overallStock);
    }
}
