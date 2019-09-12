public class Order {
    private int orderNumber;
    private char itemType;
    private int requiredUnits;

    public Order(int newOrderNumber, char newItemType, int newRequiredUnits) {
        orderNumber = newOrderNumber;
        itemType = newItemType;
        requiredUnits = newRequiredUnits;
    }

    public int getOrderNumber() {
        return orderNumber;
    }

    public char getItemType() {
        return itemType;
    }

    public int getRequiredUnits() {
        return requiredUnits;
    }
}