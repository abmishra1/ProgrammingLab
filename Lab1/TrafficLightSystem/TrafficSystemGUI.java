import javax.swing.JFrame; 
import javax.swing.JPanel; 
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JButton;
import javax.swing.table.DefaultTableModel;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.*;

public class TrafficSystemGUI {
    TrafficSignal T1;
    TrafficSignal T2;
    TrafficSignal T3;
    int currentTime;

    JFrame frame;
    JPanel pane ;
    JTable vehicleStatusTable;
    JTable trafficLightStatusTable;
    JTextField source;
    JTextField destination;
    JButton addVehicleButton;

    // Vector<Object[]> vehicleStatusList;
    // Vector<Object[]> trafficLightStatusList;

    DefaultTableModel vehicleModel;

    private Lock lock; 

    static String[] vehicleStatusColumnNames = {"Vehicle", "Source", "Destination", "Status", "Remaining Time"};
    static String[] trafficLightStatusColumnNames = {"Traffic Light", "Status", "Time"};
    
    public void acquireLock() {
        lock.lock();
    }

    public void releaseLock() {
        lock.unlock();
    }

    public void vehicleAdded(ActionEvent vehicleAddedEvent) {
        String command = vehicleAddedEvent.getActionCommand();
        // disable submit button
        if (command.equals("submit")) {
            new AddVehicleWorker(this, source.getText(), destination.getText()).execute();
            source.setText("");
            destination.setText("");
        }
        return;
    }
    
    // public TrafficSystemGUI(Object[][] vehicleStatusList, Object[][] trafficLightStatusList) {
    public TrafficSystemGUI() {
        currentTime = 0;
        T1 = new TrafficSignal(1);
        T2 = new TrafficSignal(2);
        T3 = new TrafficSignal(3);
        currentTime = 0;

        
        frame = new JFrame();
        frame.setTitle("Automatic Traffic System");
        pane = new JPanel();

        vehicleModel = new DefaultTableModel();
        vehicleModel.addColumn(vehicleStatusColumnNames);
        vehicleStatusTable = new JTable(vehicleModel);
        pane.add(vehicleStatusTable);
        
        // vehicleStatusList = new Vector<Object[]>();
        // vehicleStatusTable = new JTable(vehicleStatusList, vehicleStatusColumnNames);
        // pane.add(vehicleStatusTable);
        
        // trafficLightStatusList = new Vector<Object[]>();
        // trafficLightStatusTable = new JTable(trafficLightStatusList, trafficLightStatusColumnNames);
        // pane.add(trafficLightStatusTable);

        lock = new ReentrantLock();
        source = new JTextField(16);
        destination = new JTextField(16);
        addVehicleButton = new JButton("Add Vehicle");
        final TrafficSystemGUI x = this;

        addVehicleButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent vehicleAddedEvent) {
                System.out.println("Hello");
                new AddVehicleWorker(x,source.getText(), destination.getText()).execute();
                source.setText("");
                destination.setText("");
                // String command = vehicleAddedEvent.getActionCommand();
                // // disable submit button
                // if (command.equals("submit")) {
                //     new AddVehicleWorker(x,source.getText(), destination.getText()).execute();
                //     source.setText("");
                //     destination.setText("");
                // }
                // return;
            }
        });

        // javax.swing.Timer timer = new javax.swing.Timer(1000, new ActionListener() {
        //     public void actionPerformed(ActionEvent vehicleStatusUpdateEvent) {
        //        VehicleStatusUpdate vehicleStatusUpdtaeInstance = new VechileStatusUpdate(x);
        //         vehicleStatusUpdtaeInstance.execute();
        //     }
        //  });
        pane.add(source);
        pane.add(destination);
        pane.add(addVehicleButton);

        frame.add(pane);
        frame.setSize(720, 720);
        frame.setVisible(false);
    }

    public void showWindow() {
        frame.setVisible(true);
    }

    public int getNextPassageTime(int trafficLightNumber) {
        int nextPassageTime = 0;
        if (trafficLightNumber == 1) {
            nextPassageTime = T1.getNextPassageTime(currentTime);
        }
        if (trafficLightNumber == 2) {
            nextPassageTime = T2.getNextPassageTime(currentTime);
        }
        if (trafficLightNumber == 3) {
            nextPassageTime = T3.getNextPassageTime(currentTime);
        }
        return nextPassageTime;
    }

    // private static Object[][] getVehicleStatusList() {
    //     Object[][] vehicleStatusList = new Object[vehicles.length][5];
    //     for (int i = 0; i < vehicles.length; i++) {
    //         vehicleStatusList[i] = vehicles[i].getVehicleStatus();
    //     }
    //     return vehicleStatusList;
    // }

    public static void main(String args[]) {
        // Object[][] vehicleStatusList = getVehicleStatusList();
        TrafficSystemGUI gui = new TrafficSystemGUI();
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                gui.showWindow();
            }
        });
    }
}