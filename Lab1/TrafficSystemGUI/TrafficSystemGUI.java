/* 
    Authors' Name : Abhinav Mishra, Nitin Kedia
*/
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JScrollPane;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.BoxLayout;
import javax.swing.Timer;
import javax.swing.table.DefaultTableModel;
import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.awt.event.*;
import java.awt.Component;
import java.awt.Dimension;

public class TrafficSystemGUI {
    // 1: Data elements required in backend
    int currentTime;
    private int lastVehicleId;
    private TrafficSignal T1;
    private TrafficSignal T2;
    private TrafficSignal T3;
    DefaultTableModel vehicleModel;
    DefaultTableModel trafficLightModel;
    // Synchronisation constructs
    private ReadWriteLock timeLock; // Reader's writer's lock
    private Semaphore tableSemaphore;
    private Semaphore newVehicleSemaphore;
    private static String[] vehicleStatusColumnNames = { "Vehicle", "Source", "Destination", "Status",
            "Remaining Time" };
    private static String[] trafficLightStatusColumnNames = { "Traffic Light", "Status", "Time" };

    // 2. Data elements required for the GUI
    private JFrame frame;
    private JPanel pane;
    JTable vehicleStatusTable;
    JTable trafficLightStatusTable;
    private JTextField[] batchInputFields;
    static String[] sourceLabels = { "South", "West", "East", "South", "East", "West" };
    static String[] destinationLabels = { "East", "South", "West", "West", "South", "East" };
    private JButton addVehicleButton;
    private JLabel invalidInputLabel;

    public TrafficSystemGUI() {
        // currentTime is initialised to -1 because the time increments it
        // first then update worker executes, so -1 prevents missing out 0-1 interval
        currentTime = -1;
        lastVehicleId = 0;
        T1 = new TrafficSignal(1);
        T2 = new TrafficSignal(2);
        T3 = new TrafficSignal(3);
        // Semaphore with permit 1 means 1 thread can hold it at a time
        tableSemaphore = new Semaphore(1);
        newVehicleSemaphore = new Semaphore(1);
        // A ReadWriteLock increases concurrency in case of many readers, few writers
        // Here we have 1 writer (timer) but every updateWorker(1) and addWorker(many) reads current time
        timeLock = new ReentrantReadWriteLock();

        // Setup the GUI: two table, batch input fields etc
        // Use Layout manager instead of absolute offsets as best practice
        frame = new JFrame("Automatic Traffic System");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        pane = new JPanel();
        pane.setLayout(new BoxLayout(pane, BoxLayout.Y_AXIS));
        addTrafficLightStatusTable();
        addVehicleStatusTable();
        addBatchInputFields();
        frame.add(pane);
        frame.setSize(720, 720);
        frame.setVisible(false);
    }

    // Starts the system when called
    public void run() {
        // Initialise the timer for periodic updates
        final TrafficSystemGUI selfRef = this;
        javax.swing.Timer timer = new javax.swing.Timer(1000, new ActionListener() {
            public void actionPerformed(ActionEvent vehicleStatusUpdateEvent) {
                incrementCurrentTime(); // takes a writer's lock and increments currentTime
                // This is done to so that updates and vehicle entry are conatined in 1 sec interval
                VehicleStatusUpdate vehicleStatusUpdater = new VehicleStatusUpdate(selfRef);
                vehicleStatusUpdater.execute();
            }
        });
        timer.start();
        frame.setVisible(true);
    }

    private void addTrafficLightStatusTable() {
        // TableGUI interacts with Model which contains the data
        // Updating data in model will automatically rerender the table
        trafficLightModel = new DefaultTableModel();
        for (int i = 0; i < trafficLightStatusColumnNames.length; i++) {
            trafficLightModel.addColumn(trafficLightStatusColumnNames[i]);
        }
        // This table always has 3 rows (T1 to T3)
        trafficLightModel.addRow(T1.getTrafficSignalStatus());
        trafficLightModel.addRow(T2.getTrafficSignalStatus());
        trafficLightModel.addRow(T3.getTrafficSignalStatus());
        trafficLightStatusTable = new JTable(trafficLightModel);
        trafficLightStatusTable.setEnabled(false); // make table uneditable
        JScrollPane scrollPane = new JScrollPane(trafficLightStatusTable);
        // Add table to pane which will then be added to frame (window)
        pane.add(scrollPane);
    }

    private void addVehicleStatusTable() {
        // vehicleModel stores data of each vehicle and rerenders table
        // when a value is changed
        vehicleModel = new DefaultTableModel();
        for (int i = 0; i < vehicleStatusColumnNames.length; i++) {
            vehicleModel.addColumn(vehicleStatusColumnNames[i]);
        }
        vehicleStatusTable = new JTable(vehicleModel);
        vehicleStatusTable.setEnabled(false);
        // Vehicle list can be long, need scrollable pane
        JScrollPane scrollPane = new JScrollPane(vehicleStatusTable);
        pane.add(scrollPane);
    }

    // Adds infrastructure for taking input in batches
    private void addBatchInputFields() {
        // 1. 6 textfields for each possible direction combinations
        batchInputFields = new JTextField[6];
        JLabel[] batchInputLabels = new JLabel[6];
        JPanel rowPanel;
        for (int i = 0; i < 6; i++) {
            rowPanel = new JPanel();
            // Direction labels are stored as static items in the class
            batchInputLabels[i] = new JLabel(sourceLabels[i] + " to " + destinationLabels[i]);
            batchInputFields[i] = new JTextField("0", 8);
            batchInputLabels[i].setPreferredSize(new Dimension(100, 19));
            rowPanel.add(batchInputLabels[i]);
            rowPanel.add(batchInputFields[i]);
            pane.add(rowPanel);
        }
        
        // 2. Add a submit button for adding input vehicles to the system
        addVehicleButton = new JButton("Add Vehicle");
        final TrafficSystemGUI selfRef = this;
        addVehicleButton.addActionListener(new ActionListener() { // listen for button press
            @Override
            public void actionPerformed(ActionEvent vehicleAddedEvent) {
                // for each vehicle in each direction, start a AddVehicleWorker to service them
                // these threads will be serialised by newVehicleSemaphore which will block all
                // except one worker at a time.
                for (int i = 0; i < 6; i++) {
                    try {
                        int newVehicleCount =  Integer.parseInt(batchInputFields[i].getText());
                        for (int j = 0; j < newVehicleCount; j++) {
                            new AddVehicleWorker(selfRef, sourceLabels[i], destinationLabels[i]).execute();
                        }
                    }
                    catch (Exception error) {
                        // if input id not an interger show message in GUI
                        // which will clear after next update
                        setInvalidInputLabel(true);
                    }
                }
                for (int i = 0; i < 6; i++) {
                    batchInputFields[i].setText("0");
                }
            }
        });
        addVehicleButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        pane.add(addVehicleButton);

        invalidInputLabel = new JLabel(" ");
        invalidInputLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        pane.add(invalidInputLabel);
        return;
    }

    // Used to show a message if user doesn't enter an integer for vehicle count
    public void setInvalidInputLabel(boolean isInvalid) {
        if (isInvalid) {
            invalidInputLabel.setText("Skipping invalid counts");
        } else {
            invalidInputLabel.setText(" ");
        }
    }

    /* Following three functions are for synchronising
    current time access */
    public void getTimeReadLock() {
        // Multiple readers can be present at once, but no writers allowed
        timeLock.readLock().lock();
    }
    
    public void releaseTimeReadLock() {
        // A single writer will hold the lock if it is writing, no-one else
        timeLock.readLock().unlock();
    }

    public void incrementCurrentTime() {
        // Ensures no other reader is present before writing
        // i.e. anyone currently reading currentTime should finish
        timeLock.writeLock().lock();
        currentTime++;
        timeLock.writeLock().unlock();
    }


    /* Following two functions are used for
    synchronising vehicle status table */
    public void acquireTableSemaphore() {
        try {
            tableSemaphore.acquire();
        } catch (InterruptedException error) {
            error.printStackTrace();
        }
    }

    public void releaseTableSemaphore() {
        tableSemaphore.release();
    }

    /* Following three functions are used to
    ensure that even if there are many AddVehicleWorkers
    they will not race for id and passage time allocation */
    public void getNewVehicleSemaphore() {
        try {
            getTimeReadLock();
            newVehicleSemaphore.acquire();
        } catch (InterruptedException error) {
            error.printStackTrace();
        }
    }

    public void releaseNewVehicleSemaphore() {
        releaseTimeReadLock();
        newVehicleSemaphore.release();
    }

    public int getNewVehicleId() {
        // Serial allotment of ID's to new vehicles
        lastVehicleId++;
        return lastVehicleId;
    }

    /* Forwards requests to allot the next passage time for vehicles
        to their corresponding traffic lights and send it back */
    public int getNextPassageTime(int trafficLightNumber) {
        int nextPassageTime;
        switch (trafficLightNumber) {
            case 1:
                return T1.getNextPassageTime(currentTime);
            case 2:
                return T2.getNextPassageTime(currentTime);
            case 3:
                return T3.getNextPassageTime(currentTime);
            default:
                // non-conflicting flows can directly pass
                return currentTime;
        }
    }

    public static void main(String args[]) {
        // Initialise and run an instance of the traffic system
        TrafficSystemGUI trafficSystem = new TrafficSystemGUI();
        trafficSystem.run();
    }
}