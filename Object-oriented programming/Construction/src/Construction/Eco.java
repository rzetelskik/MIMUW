package Construction;

import java.util.ArrayList;

//Abstract class used to implement ecological and economic strategies.

public abstract class Eco implements IStrategy {

    protected abstract long optimalValue(Project project, int subset);

    protected abstract StockRod optimalRod(Project project, long sizeSum);

    /*
    This method is using a dynamic approach to find the most optimal
    solution. It saves subsets that made the solution more optimal to
    recover the best result later on.
    */
    private Order findDynamically(Project project) {
        Order order = new Order();

        long[] dp = new long[(1 << project.getRequestLength())];
        int[] dpRec = new int[(1 << project.getRequestLength())];

        dp[0] = 0;

        /*
        Iterates through masks representing the subsets and finds
        the most optimal solution for each of the subsets.
         */
        for (int mask = 1; mask < (1 << project.getRequestLength()); mask++) {
            dp[mask] = Long.MAX_VALUE;

            for (int subset = mask; subset > 0; subset = mask & (subset - 1)) {
                long optimal = optimalValue(project, subset);

                if (optimal < Integer.MAX_VALUE) {
                    long curr = dp[mask & (~subset)] + optimal;
                    if (curr < dp[mask]) {
                        dp[mask] = curr;
                        dpRec[mask] = subset;
                    }
                }
            }
        }

        int currMask = (1 << project.getRequestLength()) - 1;

        while (currMask > 0) {
            recover(project, dpRec[currMask], order);
            currMask = currMask & (~dpRec[currMask]);
        }
        return order;
    }

    //Recovers the order using the saved subsets' masks.
    private void recover(Project project, int mask, Order order) {
        ArrayList<Integer> sections = new ArrayList<>();
        int sizeSum = 0;

        for (int i = 0; i < project.getRequestLength(); i++) {
            if((mask & (1<<i)) != 0) {
                sizeSum += project.getRequest(i);
                sections.add(project.getRequest(i));
            }
        }

        orderOptimalRod(project, order, sections, sizeSum);
    }

    private void orderOptimalRod(Project project, Order order, ArrayList<Integer> sections, long sizeSum) {
        StockRod stockRod = optimalRod(project, sizeSum);
        OrderedRod newRod = new OrderedRod(stockRod);

        for (Integer i: sections) {
            newRod.cutSection(i);
        }

        order.addToOrderedRods(newRod);
    }

    public Order processRequest(Project project) {
        return findDynamically(project);
    }

}
