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

    private static int getTrafficSignalNumber(String sourceDirection, String destinationDirection) {
        if (sourceDirection.equals("South") && destinationDirection.equals("East")) {
            return 1;
        }
        if (sourceDirection.equals("West") && destinationDirection.equals("South")) {
            return 2;
        }
        if (sourceDirection.equals("East") && destinationDirection.equals("West")) {
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
        // Generate vehicle object
        trafficSystemGUI.getNewVehicleSemaphore();

        int newVehicleId = trafficSystemGUI.getNewVehicleId();
        int trafficLightNumber = getTrafficSignalNumber(sourceDirection, destinationDirection);
        int passageTime = trafficSystemGUI.getNextPassageTime(trafficLightNumber);
        String vehicleStatus = getVehicleStatus(passageTime);
        Vehicle newVehicle = new Vehicle(newVehicleId, sourceDirection, destinationDirection, vehicleStatus,
        passageTime);

        trafficSystemGUI.releaseNewVehicleSemaphore();
        return newVehicle;
    }

    @Override
    protected void done() {
        try {
            Vehicle newVehicle = get();
            // get synchronized access to the vehicle table data and
            // append this new vehicle
            trafficSystemGUI.acquireSemaphore();
            trafficSystemGUI.getTimeReadLock();
            Object[] newRow = newVehicle.getVehicleStatus(trafficSystemGUI.currentTime);
            trafficSystemGUI.vehicleModel.addRow(newRow);
            trafficSystemGUI.releaseTimeReadLock();
            trafficSystemGUI.releaseSemaphore();
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (ExecutionException e) {
            e.printStackTrace();
        }
    }
}