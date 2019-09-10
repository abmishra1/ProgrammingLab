public class Vehicle {
    int vehicleNumber;
    char source;
    char destination;
    String status;
    int passageTime;

    public Vehicle(int newvehicleNumber, char newSource, char newDestination, String newStatus, int newPassageTime) {
        vehicleNumber = newvehicleNumber;
        source = newSource;
        destination = newDestination;
        status = newStatus;
        passageTime = newPassageTime;
    }

    public Object[] getVehicleStatus(){
        Object[] vehicleStatus = { vehicleNumber, source, destination,status ,passageTime};
        return vehicleStatus;
    }
}