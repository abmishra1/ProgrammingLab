import javax.swing.JFrame; 
import javax.swing.JScrollPane; 
import javax.swing.JTable;

public class TrafficSystemGUI {
    JFrame frame;
    JTable vehicleStatusTable;
    JScrollPane scrollPane ;

    public TrafficSystemGUI(Object[][] vehicleStatusList) {
        frame = new JFrame();
        frame.setTitle("Automatic Traffic System");

        String[] columnNames = {"Vehicle", "Source", "Destination", "Status", "Remaining Time"};
        
        vehicleStatusTable = new JTable(vehicleStatusList, columnNames);
        
        scrollPane = new JScrollPane(vehicleStatusTable);
        frame.add(scrollPane);
        frame.setVisible(false);
    }

    public void showWindow() {
        frame.setVisible(true);
    }
}