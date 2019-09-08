public class Bottle {
    public int type;
    public int state;
    public int deliveryTime;

    public Bottle(int newBottleType, int newBottleState, int nextDeliveryTime){
        type = newBottleType;
        state = newBottleState;
        deliveryTime = nextDeliveryTime;
    }
}