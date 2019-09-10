import java.util.Scanner;

public class TrafficLightSystem {
    TrafficSignal T1;
    TrafficSignal T2;
    TrafficSignal T3;

    int numberOfVehicle;
    Vehicle vehicles[];

    public TrafficLightSystem() {
        T1 = new TrafficSignal(1);
        T2 = new TrafficSignal(2);
        T3 = new TrafficSignal(3);
    }

    public static int getTrafficSignalNumber(char sourceDirection, char destinationDirection) {
        if (sourceDirection == 'S' && destinationDirection == 'E') {
            return 1;
        }
        if (sourceDirection == 'W' && destinationDirection == 'S') {
            return 2;
        }
        if (sourceDirection == 'E' && destinationDirection == 'W') {
            return 3;
        }
        return -1;
    }

    public int getnextPassageTime(int trafficLightNumber) {
        int nextPassageTime = 0;
        if (trafficLightNumber == 1) {
            nextPassageTime = T1.nextPassageTime;
            T1.nextPassageTime = T1.nextPassageTime + 6;
            if (T1.nextPassageTime % 180 == 60) {
                T1.nextPassageTime = T1.nextPassageTime + 120;
            }
        }
        if (trafficLightNumber == 2) {
            nextPassageTime = T2.nextPassageTime;
            T2.nextPassageTime = T2.nextPassageTime + 6;
            if (T2.nextPassageTime % 180 == 120) {
                T2.nextPassageTime = T2.nextPassageTime + 120;
            }
        }
        if (trafficLightNumber == 3) {
            nextPassageTime = T3.nextPassageTime;
            T3.nextPassageTime = T3.nextPassageTime + 6;
            if (T3.nextPassageTime % 180 == 0) {
                T3.nextPassageTime = T3.nextPassageTime + 120;
            }
        }
        return nextPassageTime;
    }

    public String getVehicleStatus(int passageTime){
        if(passageTime == 0)
            return "Pass";
        return "Wait";
    }

    public Object[][] getVehicleStatusList() {
        Object[][] vehicleStatusList = new Object[vehicles.length][5];
        for (int i = 0; i < vehicles.length; i++) {
            vehicleStatusList[i] = vehicles[i].getVehicleStatus();
        }
        return vehicleStatusList;
    }



    public void manageTraffic() {
        Scanner inputScanner = new Scanner(System.in);
        numberOfVehicle = inputScanner.nextInt();
        vehicles = new Vehicle[numberOfVehicle];
        for (int i = 0; i < numberOfVehicle; i++) {
            char sourceDirection = inputScanner.next().charAt(0);
            inputScanner.next();
            char destinationDirection = inputScanner.next().charAt(0);
            int trafficLightNumber = getTrafficSignalNumber(sourceDirection, destinationDirection);
            int passageTime = getnextPassageTime(trafficLightNumber);
            String vehicleStatus = getVehicleStatus(passageTime);
            vehicles[i] = new Vehicle(i + 1, sourceDirection, destinationDirection, vehicleStatus, passageTime);
        }

        Object[][] vehicleStatusList = getVehicleStatusList();
        TrafficSystemGUI gui = new TrafficSystemGUI(vehicleStatusList);
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                gui.showWindow();
            }
        });
    }

    public static void main(String args[]) {
        TrafficLightSystem system = new TrafficLightSystem();
        system.manageTraffic();
    }
}