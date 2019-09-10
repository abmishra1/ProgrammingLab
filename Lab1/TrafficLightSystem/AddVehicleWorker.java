import javax.swing.SwingWorker;
import java.util.concurrent.ExecutionException;

public class AddVehicleWorker extends SwingWorker<Vehicle, Void> {
    private TrafficSystemGUI trafficSystemGUIReference;
    private String sourceDirection;
    private String destinationDirection;

    public AddVehicleWorker(TrafficSystemGUI parentReference, String sourceDirection, String destinationDirection) {
        this.trafficSystemGUIReference = parentReference;
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

    public static String getVehicleStatus(int passageTime) {
        if (passageTime == 0)
            return "Pass";
        return "Wait";
    }

    @Override
    protected Vehicle doInBackground() throws Exception {
        int trafficLightNumber = getTrafficSignalNumber(sourceDirection, destinationDirection);
        int passageTime = trafficSystemGUIReference.getNextPassageTime(trafficLightNumber);
        String vehicleStatus = getVehicleStatus(passageTime);
        
        Vehicle newVehicle = new Vehicle(100, sourceDirection, destinationDirection, vehicleStatus, passageTime);
        System.out.println("Hi");
        return newVehicle;
    }

    @Override
    protected void done() {
        try {
            Vehicle newVehicle = get();
            trafficSystemGUIReference.acquireLock();
            trafficSystemGUIReference.vehicleModel.addRow(newVehicle.getVehicleStatus());
            trafficSystemGUIReference.releaseLock();
        }
        catch (InterruptedException e) {
            e.printStackTrace();
        }
        catch (ExecutionException e) {
        // This is thrown if we throw an exception
        // from doInBackground.
        }
    }
}