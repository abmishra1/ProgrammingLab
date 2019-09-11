import javax.swing.SwingWorker;
import java.util.concurrent.ExecutionException;

public class VehicleStatusUpdate extends SwingWorker {
    private final TrafficSystemGUI trafficSystemGUIReference;

    public VehicleStatusUpdate(TrafficSystemGUI trafficSystemGUI) {
        trafficSystemGUIReference = trafficSystemGUI;
    }
    
    public void updateVehicleTable() {
        trafficSystemGUIReference.acquireLock();
        int numberOfVehicles = trafficSystemGUIReference.vehicleStatusTable.getRowCount();
        for (int i = 0; i < numberOfVehicles; i++) {
            int previousRemainingTime = (int) trafficSystemGUIReference.vehicleStatusTable.getValueAt(i,4);
            int newRemainingTime = Math.max(previousRemainingTime-1,0);
            trafficSystemGUIReference.vehicleStatusTable.setValueAt(newRemainingTime,i,4);
            if (newRemainingTime <= 0) {
                trafficSystemGUIReference.vehicleStatusTable.setValueAt("Pass",i,3);
            }
        }
        trafficSystemGUIReference.releaseLock();
    }

    public void updateTrafficSignalTable(){
        int currentCountDown = trafficSystemGUIReference.currentTime % 180;
        int activeTrafficLightNumber = (currentCountDown/60);
        for(int trafficLightNumberIndex=0; trafficLightNumberIndex<3 ; trafficLightNumberIndex++){
            if(activeTrafficLightNumber == trafficLightNumberIndex){
                int remainingCountDownTime = (60*(activeTrafficLightNumber+1)) - currentCountDown;
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("Green",trafficLightNumberIndex,1);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt(remainingCountDownTime,trafficLightNumberIndex,2);  
            }
            else{
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("Red",trafficLightNumberIndex,1);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("-",trafficLightNumberIndex,2);  
            }
        }
    }
    
    @Override
    protected Integer doInBackground() throws Exception {
        return 0;
    }
    
    @Override
    protected void done() {
        try {
            int x = (int) get();
            updateVehicleTable();
            updateTrafficSignalTable();
            trafficSystemGUIReference.setInvalidDirectionLabel(true);

        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (ExecutionException e) {
            e.printStackTrace();
        }
    }
}