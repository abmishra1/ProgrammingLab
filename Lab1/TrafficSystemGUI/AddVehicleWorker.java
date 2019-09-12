import javax.swing.SwingWorker;
import java.util.concurrent.ExecutionException;

public class AddVehicleWorker extends SwingWorker<Vehicle, Void> {
    private TrafficSystemGUI trafficSystemGUI;
    private String sourceDirection;
    private String destinationDirection;

    public AddVehicleWorker(TrafficSystemGUI parentReference, String sourceDirection, String destinationDirection) {
        this.trafficSystemGUI = parentReference;
        this.sourceDirection = sourceDirection;
        this.destinationDirection = destinationDirection;
    }

    // private static boolean isDirectionValid(String direction) {
    // return (direction.equals("South") || direction.equals("East") ||
    // direction.equals("West"));
    // }

    private static boolean isDirectionValid(String direction) {
        return (direction.equals("S") || direction.equals("E") || direction.equals("W"));
    }

    private static boolean isFlowValid(String sourceDirection, String destinationDirection) {
        if (!isDirectionValid(sourceDirection) || !isDirectionValid(destinationDirection))
            return false;
        if (sourceDirection.equals(destinationDirection))
            return false;
        return true;
    }

    // private static int getTrafficSignalNumber(String sourceDirection, String
    // destinationDirection) {
    // if (sourceDirection.equals("South") && destinationDirection.equals("East")) {
    // return 1;
    // }
    // if (sourceDirection.equals("West") && destinationDirection.equals("South")) {
    // return 2;
    // }
    // if (sourceDirection.equals("East") && destinationDirection.equals("West")) {
    // return 3;
    // }
    // return -1;
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

    public static String getVehicleStatus(int waitingTime) {
        if (waitingTime == 0)
            return "Pass";
        return "Wait";
    }

    @Override
    protected Vehicle doInBackground() throws Exception {
        // Generate vehicle object by using validated use input
        if (!isFlowValid(sourceDirection, destinationDirection)) {
            return null;
        }
        trafficSystemGUI.getNewVehicleSemaphore();
        int newVehicleId = trafficSystemGUI.getNewVehicleId();
        int trafficLightNumber = getTrafficSignalNumber(sourceDirection, destinationDirection);
        int waitingTime = trafficSystemGUI.getWaitingTime(trafficLightNumber);
        String vehicleStatus = getVehicleStatus(waitingTime);
        trafficSystemGUI.releaseNewVehicleSemaphore();
        
        Vehicle newVehicle = new Vehicle(newVehicleId, sourceDirection, destinationDirection, vehicleStatus,
                waitingTime);
        return newVehicle;
    }

    @Override
    protected void done() {
        try {
            Vehicle newVehicle = get();
            if (newVehicle == null) {
                // invalid user input
                trafficSystemGUI.setInvalidDirectionLabel(true);
                return;
            }
            // get synchronized access to the vehicle table data and
            // append this new vehicle
            trafficSystemGUI.acquireSemaphore();
            trafficSystemGUI.vehicleModel.addRow(newVehicle.getVehicleStatus());
            trafficSystemGUI.setInvalidDirectionLabel(false);
            trafficSystemGUI.releaseSemaphore();
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (ExecutionException e) {
            e.printStackTrace();
        }
    }
}