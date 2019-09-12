public class TrafficSignal {
    private int trafficLightNumber;
    private int nextPassageTime;

    public TrafficSignal(int newtrafficLightNumber) {
        trafficLightNumber = newtrafficLightNumber;
        nextPassageTime = (newtrafficLightNumber - 1) * 60;
    }

    public int getNextPassageTime(int currentTime) {
        if (currentTime > nextPassageTime) {
            if ((60 * trafficLightNumber) - (currentTime % 180) > 6) {
                nextPassageTime = currentTime;
            } else {
                nextPassageTime = (currentTime / 180 + 1) * 180 + (trafficLightNumber - 1) * 60;
            }
        }
        int allotedPassageTime = nextPassageTime;
        updateTrafficLightPassageTime();
        return allotedPassageTime;
    }

    public void updateTrafficLightPassageTime() {
        int currentPassageTime = nextPassageTime;
        nextPassageTime = nextPassageTime + 6;
        int lowerBound = (trafficLightNumber - 1) * 60;
        int upperBound = trafficLightNumber * 60 - 6;
        if ((nextPassageTime % 180) < lowerBound || (nextPassageTime % 180) > upperBound) {
            int nextRangeStart = (currentPassageTime / 180 + 1) * 180;
            nextPassageTime = nextRangeStart + (trafficLightNumber - 1) * 60;
        }
    }

    public Object[] getTrafficSignalStatus() {
        String name = "Traffic Signal " + trafficLightNumber;
        String statusColor = "Red";
        String time = "--";
        Object[] trafficSignalStatus = { name, statusColor, time };
        return trafficSignalStatus;
    }
}