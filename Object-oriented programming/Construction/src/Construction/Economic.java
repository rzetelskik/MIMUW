package Construction;

public class Economic extends Eco {

    //Finds the price of an optimal solution for a subset.
    protected long optimalValue(Project project, int mask) {
        long sizeSum = 0;
        for (int i = 0; i < project.getRequestLength(); i++) {
            if((mask & (1<<i)) != 0) {
                sizeSum += project.getRequest(i);
            }
        }

        return optimalRod(project, sizeSum).getPrice();
    }

    //Finds the most optimal solution based on the lowest price.
    protected StockRod optimalRod(Project project, long sizeSum) {
        StockRod currBest = new StockRod(Integer.MAX_VALUE, Integer.MAX_VALUE);

        for (int i = 0; i < project.getPricingLength(); i++) {
            StockRod currRod = project.getPricingRod(i);
            if (sizeSum <= currRod.getSize() && currRod.getPrice() < currBest.getPrice()) {
                currBest = currRod;
            }
        }

        return currBest;
    }

}
