public class TrafficSignal {
    int trafficNumber;
    int nextPassageTime;

    public TrafficSignal(int newTrafficNumber) {
        trafficNumber = newTrafficNumber;
        nextPassageTime = (newTrafficNumber - 1) * 60;
    }
}