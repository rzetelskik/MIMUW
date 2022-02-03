package Construction;

//Abstract class used for Minimal and Maximal approaches.
public abstract class MinMax implements IStrategy {

    //Checks whether a request can fit in any of the leftovers.
    private boolean tryFitInLeftovers(int request, Order order) {
        boolean fits = false;
        for (int j = 0; j < order.getOrderedRodsLength() && !fits; j++) {
            if (request <= order.getOrderedRod(j).getLeftovers()) {
                order.getOrderedRod(j).cutSection(request);
                fits = true;
            }
        }
        return fits;
    }

    protected abstract void orderNewRod(Project project, Order order, int request);

    public Order processRequest(Project project) {
        Order order = new Order();

        for (int i = project.getRequestLength()- 1; i >= 0; i--) {
            int request = project.getRequest(i);

            if (!tryFitInLeftovers(request, order)) {
                orderNewRod(project, order, request);
            }
        }

        return order;
    }
}
