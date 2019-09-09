public class UnfinishedTray {
    int bottle1Count;
    int bottle2Count;

    public UnfinishedTray(int newBottle1Count, int newBottle2Count) {
        bottle1Count = newBottle1Count;
        bottle2Count = newBottle2Count;
    } 

    private void decrementBottleCount(int bottleType) {
        if (bottleType == 1) {
            bottle1Count -= 1;
        }
        else {
            bottle2Count -= 1;
        }
        return;
    } 

    private boolean isBottleAvailable(int bottleType) {
        if (bottleType == 1) {
            return (bottle1Count > 0);
        }
        return (bottle2Count > 0);
    }

    public synchronized int takeBottle(int bottleType) {
        // System.out.println("taking brand new bottle");
        if (!isBottleAvailable(bottleType)) {
            int otherBottleType = (bottleType + 1) % 2;
            if (!isBottleAvailable(otherBottleType)) {
                return -1;
            }
            decrementBottleCount(otherBottleType);
            return otherBottleType;
        }
        decrementBottleCount(bottleType);
        return bottleType;
    }
}