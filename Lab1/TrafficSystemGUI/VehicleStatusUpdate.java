/* 
    Author's Name : Abhinav Mishra, Nitin Kedia
*/
import javax.swing.SwingWorker;
import java.util.concurrent.ExecutionException;

// Worker to update vehicle waiting times and traffic light status each second
// Reader of currentTime, atmost 1 instance of this class will be active at any time
public class VehicleStatusUpdate extends SwingWorker {
    private final TrafficSystemGUI trafficSystemGUI;

    public VehicleStatusUpdate(TrafficSystemGUI trafficSystemGUI) {
        this.trafficSystemGUI = trafficSystemGUI;
    }

    // Update vehicle status table, contends with AddVehicleWorker for this table
    public void updateVehicleTable() {
        // Modifying table: take semaphore first
        trafficSystemGUI.acquireTableSemaphore();
        // loop over all vehicles, decrement waiting time by one if not already zero
        // Also change status from "Wait" to "Pass" if required
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

    // Update traffic light status table
    public void updateTrafficSignalTable() {
        int currentCountDown = trafficSystemGUI.currentTime % 180;
        int activeTrafficLightNumber = (currentCountDown / 60);

        // set active light as Green, set its remaining active time, red for others
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
        // This is empty because we just loop over current table rows
        // decrement, and update them, one simple get and set
        // The looping cost will always be required in EDT thread since
        // even we have an oracle we must set element value row wise  
        return 0;
    }

    // VehicleStatusUpdate.java
    @Override
    protected void done() {
        // Take read lock on currentTime, so that
        // it can increment only after a full update
        trafficSystemGUI.getTimeReadLock();
        updateTrafficSignalTable();
        updateVehicleTable();
        trafficSystemGUI.setInvalidInputLabel(false);
        // Table update finished release sychronisation constructs
        trafficSystemGUI.releaseTimeReadLock();
    }
}