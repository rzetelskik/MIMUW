package Construction;

public class Minimal extends MinMax {

    protected void orderNewRod(Project project, Order order, int request) {
        boolean bought = false;

        for (int i = 0; i < project.getPricingLength() && !bought; i++) {
            if (request <= project.getPricingRod(i).getSize()) {
                bought = true;

                OrderedRod orderedRod = new OrderedRod(project.getPricingRod(i));
                orderedRod.cutSection(request);
                order.addToOrderedRods(orderedRod);
            }
        }
    }
}
