package Construction;

public class OutputHandler implements IOutputHandler {

    private void printSections(Order order) {
        for (int i = 0; i < order.getOrderedRodsLength(); i++) {
            OrderedRod curr = order.getOrderedRod(i);
            System.out.print(curr.getSize());

            for (int j = 0; j < curr.getSectionsLength(); j++) {
                System.out.print(" " + curr.getSection(j));
            }

            if (curr.getSectionsLength() > 0) {
                System.out.println();
            }
        }
    }

    public void handleOutput(Order order) {
        System.out.println(order.getTotalPrice());
        System.out.println(order.getTotalLeftovers());
        printSections(order);
    }
}
