public class Vehicle {
    private int vehicleNumber;
    private char source;
    private char destination;
    private String status;
    private int passageTime;

    public Vehicle(int newVehicleNumber, String newSource, String newDestination, String newStatus,
            int newPassageTime) {
        vehicleNumber = newVehicleNumber;
        source = newSource.charAt(0);
        destination = newDestination.charAt(0);
        status = newStatus;
        passageTime = newPassageTime;
    }

    public Object[] getVehicleStatus(int currentTime) {
        int remainingWaitTime;
        if (passageTime >= currentTime) {
            remainingWaitTime = passageTime - currentTime;
        }
        else {
            remainingWaitTime = 0;
        }
        Object[] vehicleStatus = { vehicleNumber, source, destination, status, remainingWaitTime };
        return vehicleStatus;
    }
}