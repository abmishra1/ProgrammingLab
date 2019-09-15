/* 
    Authors' Name : Abhinav Mishra, Nitin Kedia
*/
// Class to represent a Vehicle
public class Vehicle {
    // vehicleNumber is alloted from the system in serial
    private int vehicleNumber;
    // Source and destination directions
    private char source;
    private char destination;
    // Time at which the vehicle will be allowed to cross
    private int passageTime;

    public Vehicle(int newVehicleNumber, String newSource, String newDestination, int newPassageTime) {
        vehicleNumber = newVehicleNumber;
        source = newSource.charAt(0); // eg. S for South
        destination = newDestination.charAt(0);
        passageTime = newPassageTime;
    }

    // Based on waiting time, find if vehicle passed or not
    public String getStatus(int waitingTime) {
        if (waitingTime == 0)
            return "Pass";
        return "Wait";
    }

    // Returns an Object array of length 5 for Swing table row
    public Object[] getVehicleStatus(int currentTime) {
        // Calculate the remaining waiting time from
        // alloted passage time and current time
        int remainingWaitTime;
        if (passageTime >= currentTime) {
            remainingWaitTime = passageTime - currentTime;
        }
        else { // already passed
            remainingWaitTime = 0;
        }
        String status = getStatus(remainingWaitTime);
        // Note: last element is waiting time (as required in table);
        Object[] vehicleStatus = { vehicleNumber, source, destination, status, remainingWaitTime };
        return vehicleStatus;
    }
}