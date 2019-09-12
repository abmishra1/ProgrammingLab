public class Vehicle {
    private int vehicleNumber;
    private char source;
    private char destination;
    private String status;
    private int currentWaitingTime;

    public Vehicle(int newVehicleNumber, String newSource, String newDestination, String newStatus,
            int initialWaitingTime) {
        vehicleNumber = newVehicleNumber;
        source = newSource.charAt(0);
        destination = newDestination.charAt(0);
        status = newStatus;
        currentWaitingTime = initialWaitingTime;
    }

    public Object[] getVehicleStatus() {
        Object[] vehicleStatus = { vehicleNumber, source, destination, status, currentWaitingTime };
        return vehicleStatus;
    }
}