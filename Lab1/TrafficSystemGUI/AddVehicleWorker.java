import javax.swing.SwingWorker;
import java.util.concurrent.ExecutionException;

// Worker to service exactly 1 new vehicle
// Multiple instances of this class may be active at any given point in time
public class AddVehicleWorker extends SwingWorker<Vehicle, Void> {
    private TrafficSystemGUI trafficSystemGUI; // parent reference
    private String sourceDirection;
    private String destinationDirection;

    public AddVehicleWorker(TrafficSystemGUI parentReference, String sourceDirection, String destinationDirection) {
        this.trafficSystemGUI = parentReference;
        this.sourceDirection = sourceDirection;
        this.destinationDirection = destinationDirection;
    }

    // Find the traffic signal this vehicle maps to. eg 1 for T1
    private static int getTrafficSignalNumber(String sourceDirection, String destinationDirection) {
        // Three conflicting directions will have a corresponding light
        if (sourceDirection.equals("South") && destinationDirection.equals("East")) {
            return 1; 
        }
        if (sourceDirection.equals("West") && destinationDirection.equals("South")) {
            return 2;
        }
        if (sourceDirection.equals("East") && destinationDirection.equals("West")) {
            return 3;
        }
        // Vehicle belongs to flow which can directly pass, eg. West to East
        return -1;
    }

    // Service new vehicle in backround worker
    @Override
    protected Vehicle doInBackground() throws Exception {
        // Generating vehicle object, take a semaphore
        trafficSystemGUI.getNewVehicleSemaphore();
        // serialised ID allotment
        int newVehicleId = trafficSystemGUI.getNewVehicleId();
        int trafficLightNumber = getTrafficSignalNumber(sourceDirection, destinationDirection);
        // serialised passage time allotment
        int passageTime = trafficSystemGUI.getNextPassageTime(trafficLightNumber);
        Vehicle newVehicle = new Vehicle(newVehicleId, sourceDirection, destinationDirection, passageTime);
        // Vehicle is now ready to be added in the table, release semaphore
        trafficSystemGUI.releaseNewVehicleSemaphore();
        return newVehicle;
    }

    @Override
    protected void done() { // Update GUI table with a new row for this vehicle
        try {
            Vehicle newVehicle = get();
            // Need two locks: 1. use currentTime to calculate
            // initial waiting time, 2. to append to vehicle status table
            trafficSystemGUI.acquireTableSemaphore();
            trafficSystemGUI.getTimeReadLock();

            // Get initial status and waiting time for the vehicle
            // after that updater thread will decrement each second
            Object[] newRow = newVehicle.getVehicleStatus(trafficSystemGUI.currentTime);
            trafficSystemGUI.vehicleModel.addRow(newRow);
            
            // Release semaphore after use
            trafficSystemGUI.releaseTimeReadLock();
            trafficSystemGUI.releaseTableSemaphore();
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (ExecutionException e) {
            e.printStackTrace();
        }
    }
}