import javax.swing.SwingWorker;
import java.util.concurrent.ExecutionException;

public class VehicleStatusUpdate extends SwingWorker {
    private final TrafficSystemGUI trafficSystemGUI;

    public VehicleStatusUpdate(TrafficSystemGUI trafficSystemGUI) {
        this.trafficSystemGUI = trafficSystemGUI;
    }

    public void updateVehicleTable() {
        trafficSystemGUI.acquireSemaphore();
        int numberOfVehicles = trafficSystemGUI.vehicleStatusTable.getRowCount();
        for (int i = 0; i < numberOfVehicles; i++) {
            int previousRemainingTime = (int) trafficSystemGUI.vehicleStatusTable.getValueAt(i, 4);
            int newRemainingTime = Math.max(previousRemainingTime - 1, 0);
            trafficSystemGUI.vehicleStatusTable.setValueAt(newRemainingTime, i, 4);
            if (newRemainingTime <= 0) {
                trafficSystemGUI.vehicleStatusTable.setValueAt("Pass", i, 3);
            }
        }
        trafficSystemGUI.releaseSemaphore();
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

    @Override
    protected void done() {
        updateVehicleTable();
        updateTrafficSignalTable();
        trafficSystemGUI.setInvalidDirectionLabel(false);
    }
}