import javax.swing.SwingWorker;
import java.util.concurrent.ExecutionException;

public class AddVehicleWorker extends SwingWorker<Vehicle, Void> {
    private TrafficSystemGUI trafficSystemGUIReference;
    private String sourceDirection;
    private String destinationDirection;

    public AddVehicleWorker(TrafficSystemGUI parentReference, String sourceDirection, String destinationDirection) {
        this.trafficSystemGUIReference = parentReference;
        this.sourceDirection = sourceDirection;
        this.destinationDirection = destinationDirection;
    }


    // private static boolean isDirectionValid(String direction) {
    //     return (direction.equals("South") || direction.equals("East") || direction.equals("West"));
    // }

    private static boolean isDirectionValid(String direction) {
        return (direction.equals("S") || direction.equals("E") || direction.equals("W"));
    }

    private static boolean isFlowValid(String sourceDirection, String destinationDirection) {
        if (!isDirectionValid(sourceDirection) || !isDirectionValid(destinationDirection)) return false;
        if (sourceDirection.equals(destinationDirection)) return false;
        return true;
    }

    // private static int getTrafficSignalNumber(String sourceDirection, String destinationDirection) {
    //     if (sourceDirection.equals("South") && destinationDirection.equals("East")) {
    //         return 1;
    //     }
    //     if (sourceDirection.equals("West") && destinationDirection.equals("South")) {
    //         return 2;
    //     }
    //     if (sourceDirection.equals("East") && destinationDirection.equals("West")) {
    //         return 3;
    //     }
    //     return -1;
    // }

    private static int getTrafficSignalNumber(String sourceDirection, String destinationDirection) {
        if (sourceDirection.equals("S") && destinationDirection.equals("E")) {
            return 1;
        }
        if (sourceDirection.equals("W") && destinationDirection.equals("S")) {
            return 2;
        }
        if (sourceDirection.equals("E") && destinationDirection.equals("W")) {
            return 3;
        }
        return -1;
    }

    public static String getVehicleStatus(int passageTime) {
        if (passageTime == 0)
            return "Pass";
        return "Wait";
    }

    @Override
    protected Vehicle doInBackground() throws Exception {
        if (!isFlowValid(sourceDirection, destinationDirection)) {
            return null;
        }
        int newVehicleId = trafficSystemGUIReference.getNewVehicleId();
        int trafficLightNumber = getTrafficSignalNumber(sourceDirection, destinationDirection);
        int passageTime = trafficSystemGUIReference.getNextPassageTime(trafficLightNumber);
        String vehicleStatus = getVehicleStatus(passageTime);
        System.out.println("Vehicle " + newVehicleId + " " + trafficSystemGUIReference.currentTime + " " + passageTime);
        passageTime -= trafficSystemGUIReference.currentTime;
        Vehicle newVehicle = new Vehicle(newVehicleId, sourceDirection, destinationDirection, vehicleStatus, passageTime);
        return newVehicle;
    }

    @Override
    protected void done() {
        try {
            Vehicle newVehicle = get();
            if (newVehicle == null) {
                System.out.println("Hi");
                trafficSystemGUIReference.setInvalidDirectionLabel(false);
                return;
            }
            trafficSystemGUIReference.acquireLock();
            trafficSystemGUIReference.vehicleModel.addRow(newVehicle.getVehicleStatus());
            trafficSystemGUIReference.setInvalidDirectionLabel(true);
            trafficSystemGUIReference.releaseLock();
        }
        catch (InterruptedException e) {
            e.printStackTrace();
        }
        catch (ExecutionException e) {
            e.printStackTrace();
        }
    }
}