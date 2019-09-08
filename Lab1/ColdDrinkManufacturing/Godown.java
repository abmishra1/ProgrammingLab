public class Godown {
    int bottle1Count;
    int bottle2Count;

    public Godown() {
        bottle1Count = 0;
        bottle2Count = 0;
    }

    public boolean storeBottle(int bottleType) {
        if (bottleType == 1) {
            bottle1Count++;
            return true;
        }
        bottle2Count++;
        return true;
    }

    public int getBottleCount(int bottleType) {
        if (bottleType == 1) {
            return bottle1Count;
        }
        return bottle2Count;
    }
}