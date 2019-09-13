// Class representing a traffic signal
public class TrafficSignal {
    private int trafficLightNumber;
    // the time which will be allotted to the next vehicle which
    // maps to this traffic light
    private int nextPassageTime;
    private static int cycleDuration = 180; // 3 lights with 60s green time each

    public TrafficSignal(int newtrafficLightNumber) {
        trafficLightNumber = newtrafficLightNumber;
        // first passage time will be first time when signal turns green, eg. 60 for T2
        nextPassageTime = (newtrafficLightNumber - 1) * 60;
    }

    // Function used to allot passage time for a vehicle
    public int getNextPassageTime(int currentTime) {
        // nextPassageTime may have been expired, if so currentTime will be used 
        if (currentTime > nextPassageTime) {
            // if this traffic light is green and will be for 6 more sec, allot current time
            if ((60 * trafficLightNumber) - (currentTime % cycleDuration) > 6) {
                nextPassageTime = currentTime;
            }
            else { // allot time when this signal will be green next
                int nextRangeStart = (currentTime / cycleDuration + 1) * cycleDuration;
                nextPassageTime = nextRangeStart + (trafficLightNumber - 1) * 60;
            }
        }
        int allotedPassageTime = nextPassageTime;
        // This vehicle has been allocated, calculate for next vehicle
        updateTrafficLightPassageTime();
        return allotedPassageTime;
    }

    // After alloting for current vehicle, update nextPaasageTime for next vehicle
    public void updateTrafficLightPassageTime() {
        int currentPassageTime = nextPassageTime;
        // By default allot after 6 seconds (time taken by vehicle to pass)
        // But this signal must be green then for another 6 sec
        nextPassageTime = nextPassageTime + 6;
        int lowerBound = (trafficLightNumber - 1) * 60;
        int upperBound = trafficLightNumber * 60 - 6; 
        // minus 6 to check signal remains green till this vehicle passes
        if ((nextPassageTime % cycleDuration) < lowerBound || (nextPassageTime % cycleDuration) > upperBound) {
            // if less than 6 seconds remain in this slot, assign the start of the next slot
            int nextRangeStart = (currentPassageTime / cycleDuration + 1) * cycleDuration;
            nextPassageTime = nextRangeStart + (trafficLightNumber - 1) * 60;
        }
    }

    // Return the inital Object array of size 3 for Swing traffic lights table 
    public Object[] getTrafficSignalStatus() {
        // Simulation hasn't started so every light is RED
        String name = "Traffic Signal " + trafficLightNumber;
        String statusColor = "Red";
        String time = "--";
        Object[] trafficSignalStatus = { name, statusColor, time };
        return trafficSignalStatus;
    }
}