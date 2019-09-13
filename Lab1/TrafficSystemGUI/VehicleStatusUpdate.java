import javax.swing.SwingWorker;
import java.util.concurrent.ExecutionException;

public class VehicleStatusUpdate extends SwingWorker {
    private final TrafficSystemGUI trafficSystemGUI;

    public VehicleStatusUpdate(TrafficSystemGUI trafficSystemGUI) {
        this.trafficSystemGUI = trafficSystemGUI;
    }

    // VehicleStatusUpdate.java
    public void updateVehicleTable() {
        // Modyfying table: take semaphore first
        trafficSystemGUI.acquireTableSemaphore();
        // loop over all vehicles, decrement waiting time by one if not already passed
        // Change status from "Wait" to "Pass" if required
        int numberOfVehicles = trafficSystemGUI.vehicleStatusTable.getRowCount();
        for (int i = 0; i < numberOfVehicles; i++) {
            int previousRemainingTime = (int) trafficSystemGUI.vehicleStatusTable.getValueAt(i, 4);
            int newRemainingTime = Math.max(previousRemainingTime - 1, 0);
            trafficSystemGUI.vehicleStatusTable.setValueAt(newRemainingTime, i, 4);
            if (newRemainingTime <= 0) {
                trafficSystemGUI.vehicleStatusTable.setValueAt("Pass", i, 3);
            }
        }
        // Table updated: release semaphore
        trafficSystemGUI.releaseTableSemaphore();
    }

    public void updateTrafficSignalTable() {
        int currentCountDown = trafficSystemGUI.currentTime % 180;
        int activeTrafficLightNumber = (currentCountDown / 60);
        for (int trafficLightNumber = 0; trafficLightNumber < 3; trafficLightNumber++) {
            if (activeTrafficLightNumber == trafficLightNumber) {
                int remainingCountDownTime = (60 * (activeTrafficLightNumber + 1)) - currentCountDown;
                trafficSystemGUI.trafficLightStatusTable.setValueAt("Green", trafficLightNumber, 1);
                trafficSystemGUI.trafficLightStatusTable.setValueAt(remainingCountDownTime, trafficLightNumber, 2);
            } else {
                trafficSystemGUI.trafficLightStatusTable.setValueAt("Red", trafficLightNumber, 1);
                trafficSystemGUI.trafficLightStatusTable.setValueAt("--", trafficLightNumber, 2);
            }
        }
    }

    @Override
    protected Integer doInBackground() throws Exception {
        return 0;
    }

    // VehicleStatusUpdate.java
    @Override
    protected void done() {
        // Take read lock on currentTime, so that
        // it can increments only after a full update
        trafficSystemGUI.getTimeReadLock();
        updateVehicleTable();
        updateTrafficSignalTable();
        trafficSystemGUI.setInvalidDirectionLabel(false);
        trafficSystemGUI.releaseTimeReadLock();
    }
}