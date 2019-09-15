/* 
    Authors' Name : Abhinav Mishra, Nitin Kedia
    Description: This class represents a bottle instance
*/  
public class Bottle {
    // the bottle type 1 or 2
    public int type;
    // state whether it is packaaged, sealed , both or none
    public int state;
    // at what time wll be the current ongoing opertation (packaging/sealing) will be complete
    public int deliveryTime;


    // Constructor to initialize the bttles with initial values
    public Bottle(int newBottleType, int newBottleState, int nextDeliveryTime){
        type = newBottleType;
        state = newBottleState;
        deliveryTime = nextDeliveryTime;
    }
}