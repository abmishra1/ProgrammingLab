public class TrafficSignal {
    int trafficLightNumber;
    int nextPassageTime;

    public TrafficSignal(int newtrafficLightNumber) {
        trafficLightNumber = newtrafficLightNumber;
        nextPassageTime = (newtrafficLightNumber - 1) * 60;
    }

    public int getNextPassageTime(int currentTime) {
        if (currentTime > nextPassageTime){
            nextPassageTime = (currentTime % 180) < 60 * trafficLightNumber ? currentTime
                : (currentTime / 180 + 1) * 180 + (trafficLightNumber - 1) * 60;
        }
        int allotedPassageTime = nextPassageTime;
        updateTrafficLightPassageTime();
        return allotedPassageTime;
    }

    public void updateTrafficLightPassageTime() {
        nextPassageTime = nextPassageTime + 6;
        if (nextPassageTime % 180 >= (60 * trafficLightNumber) % 180) {
            nextPassageTime = (nextPassageTime/180 + 1)*180 + (trafficLightNumber - 1) * 60;
            System.out.println("Exceed kiya naya next passage time : "+ nextPassageTime);
        }
    }

    public Object[] getTrafficSignalStatus() {
        String name = "Traffic Signal " + trafficLightNumber;
        String statusColor = "Red";
        String time = "--";
        Object[] trafficSignalStatus = {name, statusColor, time};
        return trafficSignalStatus;
    }
}