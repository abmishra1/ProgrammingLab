public class Order {
    public int orderNumber;
    public char itemType;
    public int requiredUnits;

    public Order(int newOrderNumber, char newItemType, int newRequiredUnits) {
        orderNumber = newOrderNumber;
        itemType = newItemType;
        requiredUnits = newRequiredUnits;
    }
}