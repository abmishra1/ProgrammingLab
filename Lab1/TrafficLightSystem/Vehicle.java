public class Vehicle {
    int vehicleNumber;
    char source;
    char destination;
    String status;
    int passageTime;

    public Vehicle(int newvehicleNumber, String newSource, String newDestination, String newStatus, int newPassageTime) {
        vehicleNumber = newvehicleNumber;
        source = newSource.charAt(0);
        destination = newDestination.charAt(0);
        status = newStatus;
        passageTime = newPassageTime;
    }

    public Object[] getVehicleStatus(){
        Object[] vehicleStatus = { vehicleNumber, source, destination,status ,passageTime};
        return vehicleStatus;
    }
}