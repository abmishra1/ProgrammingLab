/* 
    Author's Name : Abhinav Mishra, Nitin Kedia
    Description: Class for representing one order thereby for one item type
*/
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