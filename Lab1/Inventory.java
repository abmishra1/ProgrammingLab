import java.util.HashMap;

public class Inventory {
    HashMap<Character, Integer> itemStockMap;

    public Inventory(int[] itemCountArray) {
        if (itemCountArray.length == 4) {
            itemStockMap = new HashMap<Character, Integer>();
            itemStockMap.put('S', itemCountArray[0]);
            itemStockMap.put('M', itemCountArray[1]);
            itemStockMap.put('L', itemCountArray[2]);
            itemStockMap.put('C', itemCountArray[3]);
        }
        else {
            throw new IllegalArgumentException("Exactly 4 item counts needed.");
        }        
    }

    private boolean checkItemStock(char itemType, int requiredUnits) {
        if (itemStockMap.containsKey(itemType)) {
            int currentStock = itemStockMap.get(itemType);
            return requiredUnits <= currentStock;
        }
        else {
            throw new IllegalArgumentException("Unidentified Item Type");
        }
    }

    private void printInventory() {
        System.out.println("Current Inventory: ");
        for (HashMap.Entry<Character, Integer> itemStockEntry : itemStockMap.entrySet()) {
            System.out.print(itemStockEntry.getKey() + ": " + itemStockEntry.getValue() + " ");
        }
        System.out.println(); // shift to new line
        return;
    }

    public synchronized void updateItemCount(Order order) {
        System.out.println(); // shift to new line
        if (checkItemStock(order.itemType, order.requiredUnits) == false) {
            System.out.println("Order Number " + order.orderNumber + " failed due to insufficient stock.");
            return;
        }
        int currentStock = itemStockMap.get(order.itemType);
        itemStockMap.put(order.itemType, currentStock - order.requiredUnits);
        System.out.println("Order Number " + order.orderNumber + " successful.");
        printInventory();
        return;
    }
}