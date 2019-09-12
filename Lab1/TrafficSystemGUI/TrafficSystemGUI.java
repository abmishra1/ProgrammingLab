import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JScrollPane;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.BoxLayout;
import javax.swing.table.DefaultTableModel;
import java.util.concurrent.Semaphore;
import java.awt.event.*;
import java.awt.Component;
import java.awt.Dimension;

public class TrafficSystemGUI {
    int currentTime;
    private int lastVehicleId;
    private TrafficSignal T1;
    private TrafficSignal T2;
    private TrafficSignal T3;
    DefaultTableModel vehicleModel;
    DefaultTableModel trafficLightModel;
    private Semaphore semaphore;
    private static String[] vehicleStatusColumnNames = { "Vehicle", "Source", "Destination", "Status",
            "Remaining Time" };
    private static String[] trafficLightStatusColumnNames = { "Traffic Light", "Status", "Time" };

    private JFrame frame;
    private JPanel pane;
    JTable vehicleStatusTable;
    JTable trafficLightStatusTable;
    private JTextField source;
    private JTextField destination;
    private JButton addVehicleButton;
    private JLabel invalidDirectionLabel;

    public TrafficSystemGUI() {
        currentTime = 0;
        lastVehicleId = 0;
        T1 = new TrafficSignal(1);
        T2 = new TrafficSignal(2);
        T3 = new TrafficSignal(3);
        semaphore = new Semaphore(1);

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
        trafficLightStatusTable.setEnabled(false);
        JScrollPane scrollPane1 = new JScrollPane(trafficLightStatusTable);

        pane.add(scrollPane1);

        vehicleModel = new DefaultTableModel();
        for (int i = 0; i < vehicleStatusColumnNames.length; i++) {
            vehicleModel.addColumn(vehicleStatusColumnNames[i]);
        }
        vehicleStatusTable = new JTable(vehicleModel);
        vehicleStatusTable.setEnabled(false);
        JScrollPane scrollPane2 = new JScrollPane(vehicleStatusTable);
        pane.add(scrollPane2);

        JPanel bottomPanel = new JPanel();
        source = new JTextField(16);
        source.setMaximumSize(source.getPreferredSize());
        destination = new JTextField(16);
        destination.setMaximumSize(destination.getPreferredSize());
        addVehicleButton = new JButton("Add Vehicle");

        final TrafficSystemGUI selfRef = this;

        addVehicleButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent vehicleAddedEvent) {
                new AddVehicleWorker(selfRef, source.getText(), destination.getText()).execute();
                source.setText("");
                destination.setText("");
            }
        });

        javax.swing.Timer timer = new javax.swing.Timer(1000, new ActionListener() {
            public void actionPerformed(ActionEvent vehicleStatusUpdateEvent) {
                currentTime++;
                VehicleStatusUpdate vehicleStatusUpdtaeInstance = new VehicleStatusUpdate(selfRef);
                vehicleStatusUpdtaeInstance.execute();
            }
        });
        timer.start();

        bottomPanel.add(source);
        bottomPanel.add(destination);
        bottomPanel.add(addVehicleButton);
        pane.add(bottomPanel);

        invalidDirectionLabel = new JLabel(" ");
        invalidDirectionLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        pane.add(invalidDirectionLabel);

        frame.add(pane);
        frame.setSize(720, 720);
        frame.setVisible(false);
    }

    public int getCurrentTime() {
        return currentTime;
    }

    public void acquireSemaphore() {
        try {
            semaphore.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public void releaseSemaphore() {
        semaphore.release();
    }

    public int getNewVehicleId() {
        lastVehicleId++;
        return lastVehicleId;
    }

    public void setInvalidDirectionLabel(boolean isInvalid) {
        if (isInvalid) {
            invalidDirectionLabel.setText("Invalid directions entered");
        } else {
            invalidDirectionLabel.setText(" ");
        }
    }

    public int getNextPassageTime(int trafficLightNumber) {
        int nextPassageTime;
        if (trafficLightNumber == 1) {
            nextPassageTime = T1.getNextPassageTime(currentTime);
        } else if (trafficLightNumber == 2) {
            nextPassageTime = T2.getNextPassageTime(currentTime);
        } else if (trafficLightNumber == 3) {
            nextPassageTime = T3.getNextPassageTime(currentTime);
        } else {
            nextPassageTime = currentTime;
        }
        return nextPassageTime;
    }

    public void showWindow() {
        frame.setVisible(true);
    }

    public static void main(String args[]) {
        TrafficSystemGUI trafficSystem = new TrafficSystemGUI();
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                trafficSystem.showWindow();
            }
        });
    }
}