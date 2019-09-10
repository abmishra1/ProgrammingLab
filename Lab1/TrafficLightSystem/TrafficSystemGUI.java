import javax.swing.JFrame; 
import javax.swing.JPanel; 
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JScrollPane;
import javax.swing.JButton;
import javax.swing.BoxLayout;
import javax.swing.table.DefaultTableModel;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.awt.event.*;
import java.awt.Component;

public class TrafficSystemGUI {
    int currentTime;
    private int lastVehicleId;
    TrafficSignal T1;
    TrafficSignal T2;
    TrafficSignal T3;
    DefaultTableModel vehicleModel;
    DefaultTableModel trafficLightModel;
    private Lock lock; 
    static String[] vehicleStatusColumnNames = {"Vehicle", "Source", "Destination", "Status", "Remaining Time"};
    static String[] trafficLightStatusColumnNames = {"Traffic Light", "Status", "Time"};

    JFrame frame;
    JPanel pane ;
    JTable vehicleStatusTable;
    JTable trafficLightStatusTable;
    JTextField source;
    JTextField destination;
    JButton addVehicleButton;

    public void acquireLock() {
        lock.lock();
    }

    public void releaseLock() {
        lock.unlock();
    }

    public int getNewVehicleId() {
        lastVehicleId++;
        return lastVehicleId;
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
    
    public TrafficSystemGUI() {
        currentTime = 0;
        lastVehicleId = 0;
        T1 = new TrafficSignal(1);
        T2 = new TrafficSignal(2);
        T3 = new TrafficSignal(3);
        lock = new ReentrantLock();
        
        frame = new JFrame("Automatic Traffic System");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        pane = new JPanel();
        pane.setLayout(new BoxLayout(pane, BoxLayout.Y_AXIS));

        trafficLightModel = new DefaultTableModel();
        for (int i = 0; i < trafficLightStatusColumnNames.length; i++) {
            trafficLightModel.addColumn(trafficLightStatusColumnNames[i]);
        }
        trafficLightModel.addRow(T1.getTrafficSignalStatus());
        trafficLightModel.addRow(T2.getTrafficSignalStatus());
        trafficLightModel.addRow(T3.getTrafficSignalStatus());
        trafficLightStatusTable = new JTable(trafficLightModel);
        JScrollPane scrollPane2 = new JScrollPane(trafficLightStatusTable);
        pane.add(scrollPane2); 

        vehicleModel = new DefaultTableModel();
        for (int i = 0; i < vehicleStatusColumnNames.length; i++) {
            vehicleModel.addColumn(vehicleStatusColumnNames[i]);
        }
        vehicleStatusTable = new JTable(vehicleModel);
        JScrollPane scrollPane = new JScrollPane(vehicleStatusTable);
        pane.add(scrollPane);


        source = new JTextField(16);
        source.setMaximumSize( source.getPreferredSize() );
        source.setAlignmentX(Component.CENTER_ALIGNMENT);
        destination = new JTextField(16);
        destination.setMaximumSize( destination.getPreferredSize() );        
        destination.setAlignmentX(Component.CENTER_ALIGNMENT);
        addVehicleButton = new JButton("Add Vehicle");
        addVehicleButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        final TrafficSystemGUI x = this;

        addVehicleButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent vehicleAddedEvent) {
                new AddVehicleWorker(x,source.getText(), destination.getText()).execute();
                source.setText("");
                destination.setText("");
            }
        });

        javax.swing.Timer timer = new javax.swing.Timer(3000, new ActionListener() {
            public void actionPerformed(ActionEvent vehicleStatusUpdateEvent) {
                currentTime++;
                VehicleStatusUpdate vehicleStatusUpdtaeInstance = new VehicleStatusUpdate(x);
                vehicleStatusUpdtaeInstance.execute();
            }
         });
        timer.start();

        pane.add(source);
        pane.add(destination);
        pane.add(addVehicleButton);

        frame.add(pane);
        frame.setSize(720, 720);
        frame.setVisible(false);
    }

    public int getNextPassageTime(int trafficLightNumber) {
        int nextPassageTime;
        if (trafficLightNumber == 1) {
            nextPassageTime = T1.getNextPassageTime(currentTime);
        }
        else if (trafficLightNumber == 2) {
            nextPassageTime = T2.getNextPassageTime(currentTime);
        }
        else if (trafficLightNumber == 3) {
            nextPassageTime = T3.getNextPassageTime(currentTime);
        }
        else {
            nextPassageTime = currentTime;
        }
        return nextPassageTime;
    }

    public void showWindow() {
        frame.setVisible(true);
    }

    public static void main(String args[]) {
        TrafficSystemGUI gui = new TrafficSystemGUI();
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                gui.showWindow();
            }
        });
    }
}