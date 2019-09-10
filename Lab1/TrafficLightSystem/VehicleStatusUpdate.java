import javax.swing.SwingWorker;

public class VehicleStatusUpdate extends SwingWorker {

    private final TrafficSystemGUI trafficSystemGUIReference;

    public VehicleStatusUpdate(TrafficSystemGUI trafficSystemGUI) {
        trafficSystemGUIReference = trafficSystemGUI;
    }

    @Override
    protected Void doInBackground() throws Exception {
        trafficSystemGUIReference.acquireLock();
        int numberOfVehicles = trafficSystemGUIReference.vehicleStatusList.length;
        for (int i = 0; i < numberOfVehicles; i++) {
            trafficSystemGUIReference.vehicleStatusList[i][4]--;
            if (trafficSystemGUIReference.vehicleStatusList[i][4] <= 0) {
                trafficSystemGUIReference.vehicleStatusList[i][3] = "Pass";
            }
        }
        trafficSystemGUIReference.releaseLock();
        return Void;
    }

    @Override
    protected void done() {
        try {
            get();
            trafficSystemGUIReference.vehicleStatusTable.repaint();

        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (ExecutionException e) {
            e.printStackTrace();
        }
    }
}