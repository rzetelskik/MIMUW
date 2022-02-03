package Construction;

public class Maximal extends MinMax {

    protected void orderNewRod(Project project, Order order, int request) {
        int j = project.getPricingLength() - 1;

        OrderedRod orderedRod = new OrderedRod(project.getPricingRod(j));
        orderedRod.cutSection(request);
        order.addToOrderedRods(orderedRod);
    }

}
