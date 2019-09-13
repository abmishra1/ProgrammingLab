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
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
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
    private ReadWriteLock timeLock; // Reader's writer's lock
    private Semaphore semaphore;
    private Semaphore newVehicleSemaphore;
    private static String[] vehicleStatusColumnNames = { "Vehicle", "Source", "Destination", "Status",
            "Remaining Time" };
    private static String[] trafficLightStatusColumnNames = { "Traffic Light", "Status", "Time" };

    private JFrame frame;
    private JPanel pane;
    JTable vehicleStatusTable;
    JTable trafficLightStatusTable;
    private JTextField[] batchInputBoxes;
    static String[] sourceLabels = { "South", "West", "East", "South", "East", "West" };
    static String[] destinationLabels = { "East", "South", "West", "West", "South", "East" };
    private JButton addVehicleButton;
    private JLabel invalidDirectionLabel;

    public TrafficSystemGUI() {
        currentTime = 0;
        lastVehicleId = 0;
        T1 = new TrafficSignal(1);
        T2 = new TrafficSignal(2);
        T3 = new TrafficSignal(3);
        semaphore = new Semaphore(1);
        newVehicleSemaphore = new Semaphore(1);
        timeLock = new ReentrantReadWriteLock();

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

        addBatchInputBoxes();
        final TrafficSystemGUI selfRef = this;
        javax.swing.Timer timer = new javax.swing.Timer(1000, new ActionListener() {
            public void actionPerformed(ActionEvent vehicleStatusUpdateEvent) {
                incrementCurrentTime();
                VehicleStatusUpdate vehicleStatusUpdateInstance = new VehicleStatusUpdate(selfRef);
                vehicleStatusUpdateInstance.execute();
            }
        });
        timer.start();

        frame.add(pane);
        frame.setSize(720, 720);
        frame.setVisible(false);
    }

    private void addBatchInputBoxes() {
        batchInputBoxes = new JTextField[6];
        JLabel[] batchInputLabels = new JLabel[6];
        JPanel rowPanel;
        for (int i = 0; i < 6; i++) {
            rowPanel = new JPanel();
            batchInputLabels[i] = new JLabel(sourceLabels[i] + " to " + destinationLabels[i]);
            batchInputBoxes[i] = new JTextField("0", 8);
            batchInputLabels[i].setPreferredSize(new Dimension(100, 19));
            rowPanel.add(batchInputLabels[i]);
            rowPanel.add(batchInputBoxes[i]);
            pane.add(rowPanel);
        }
        
        addVehicleButton = new JButton("Add Vehicle");
        final TrafficSystemGUI selfRef = this;
        addVehicleButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent vehicleAddedEvent) {
                for (int i = 0; i < 6; i++) {
                    try {
                        int newVehicleCount =  Integer.parseInt(batchInputBoxes[i].getText());
                        for (int j = 0; j < newVehicleCount; j++) {
                            new AddVehicleWorker(selfRef, sourceLabels[i], destinationLabels[i]).execute();
                        }
                    }
                    catch (Exception error) {
                        setInvalidDirectionLabel(true);
                    }
                }
                for (int i = 0; i < 6; i++) {
                    batchInputBoxes[i].setText("0");
                }
            }
        });
        addVehicleButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        pane.add(addVehicleButton);

        invalidDirectionLabel = new JLabel(" ");
        invalidDirectionLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        pane.add(invalidDirectionLabel);
        return;
    }

    public void getTimeReadLock() {
        timeLock.readLock().lock();
    }

    public void releaseTimeReadLock() {
        timeLock.readLock().unlock();
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

    public void incrementCurrentTime() {
        timeLock.writeLock().lock();
        currentTime++;
        timeLock.writeLock().unlock();
    }

    public void getNewVehicleSemaphore() {
        try {
            newVehicleSemaphore.acquire();
            getTimeReadLock();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public void releaseNewVehicleSemaphore() {
        newVehicleSemaphore.release();
        releaseTimeReadLock();
    }

    public int getNewVehicleId() {
        lastVehicleId++;
        return lastVehicleId;
    }

    public void setInvalidDirectionLabel(boolean isInvalid) {
        // return;
        // // REMOVE THIS RETURN
        if (isInvalid) {
            invalidDirectionLabel.setText("Skipping invalid counts");
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