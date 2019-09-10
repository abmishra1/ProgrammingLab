public class TrafficSignal {
    int trafficNumber;
    int nextPassageTime;

    public TrafficSignal(int newTrafficNumber) {
        trafficNumber = newTrafficNumber;
        nextPassageTime = (newTrafficNumber - 1) * 60;
    }

    public int getNextPassageTime(int currentTime) {
        if (currentTime <= nextPassageTime)
            return nextPassageTime;

        nextPassageTime = (currentTime % 180) < 60 * trafficNumber ? currentTime
                : (currentTime / 180 + 1) * 180 + (trafficNumber - 1) * 60;
        
        int allotedPassageTime = nextPassageTime;
        updateTrafficLightPassageTime();
        return allotedPassageTime;
    }

    public void updateTrafficLightPassageTime() {
        nextPassageTime = nextPassageTime + 6;
        if (nextPassageTime % 180 == (60 * trafficNumber) % 180) {
            nextPassageTime = nextPassageTime + 120;
        }
    }
}