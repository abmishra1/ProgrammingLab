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

    // AddVehicleWorker.java
    @Override
    protected Vehicle doInBackground() throws Exception {
        // Generating vehicle object, take a semaphore
        trafficSystemGUI.getNewVehicleSemaphore();
        // serialised ID allotment
        int newVehicleId = trafficSystemGUI.getNewVehicleId();
        int trafficLightNumber = getTrafficSignalNumber(sourceDirection, destinationDirection);
        // serialised passage time allotment
        int passageTime = trafficSystemGUI.getNextPassageTime(trafficLightNumber);
        String vehicleStatus = getVehicleStatus(passageTime);
        Vehicle newVehicle = new Vehicle(newVehicleId, sourceDirection, destinationDirection, vehicleStatus,
        passageTime);
        // Vehicle is now ready to be added in the table, release semaphore
        trafficSystemGUI.releaseNewVehicleSemaphore();
        return newVehicle;
    }

    // AddVehicleWorker.java
    @Override
    protected void done() {
        try {
            Vehicle newVehicle = get();
            // Need two locks: 1. use currentTime to calculate
            // initial waiting time, 2. to append to vehicle list
            trafficSystemGUI.acquireTableSemaphore();
            trafficSystemGUI.getTimeReadLock();
            Object[] newRow = newVehicle.getVehicleStatus(trafficSystemGUI.currentTime);
            trafficSystemGUI.vehicleModel.addRow(newRow);
            trafficSystemGUI.releaseTimeReadLock();
            trafficSystemGUI.releaseTableSemaphore();
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (ExecutionException e) {
            e.printStackTrace();
        }
    }
}