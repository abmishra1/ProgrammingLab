import javax.swing.SwingWorker;
import java.util.concurrent.ExecutionException;

public class VehicleStatusUpdate extends SwingWorker {
    private final TrafficSystemGUI trafficSystemGUIReference;

    public VehicleStatusUpdate(TrafficSystemGUI trafficSystemGUI) {
        trafficSystemGUIReference = trafficSystemGUI;
    }
    
    @Override
    protected Integer doInBackground() throws Exception {
        return 1;
    }
    
    @Override
    protected void done() {
        try {
            int x = (int) get();
            trafficSystemGUIReference.acquireLock();
            int numberOfVehicles = trafficSystemGUIReference.vehicleStatusTable.getRowCount();
            for (int i = 0; i < numberOfVehicles; i++) {
                int previousRemainingTime = (int) trafficSystemGUIReference.vehicleStatusTable.getValueAt(i,4);
                int newRemainingTime = Math.max(previousRemainingTime-1,0);
                trafficSystemGUIReference.vehicleStatusTable.setValueAt(newRemainingTime,i,4);
                // trafficSystemGUIReference.vehicleStatusList[i][4]--;
                if (newRemainingTime <= 0) {
                    trafficSystemGUIReference.vehicleStatusTable.setValueAt("Pass",i,3);
                }
            }
            trafficSystemGUIReference.releaseLock();

            int currentCountDown = trafficSystemGUIReference.currentTime % 180;
            if(currentCountDown < 60){
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt(60-currentCountDown,0,2);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("Green",0,1);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("-",1,2);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("Red",1,1);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("-",2,2);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("Red",2,1);
            }
            else if(currentCountDown < 120){
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("-",0,2);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("Red",0,1);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt(120-currentCountDown,1,2);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("Green",1,1);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("-",2,2);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("Red",2,1);
            }
            else{
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("-",0,2);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("Red",0,1);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt(120-currentCountDown,1,2);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("Red",1,1);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt(180-currentCountDown,2,2);
                trafficSystemGUIReference.trafficLightStatusTable.setValueAt("Green",2,1);
            }

        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (ExecutionException e) {
            e.printStackTrace();
        }
    }
}