package Construction;

import java.util.ArrayList;

public class Order {
    private ArrayList<OrderedRod> orderedRods;
    private long totalLeftovers;
    private long totalPrice;

    public Order() {
        orderedRods = new ArrayList<>();
        totalLeftovers = 0;
        totalPrice = 0;
    }

    public OrderedRod getOrderedRod(int i) {
        return orderedRods.get(i);
    }

    public int getOrderedRodsLength() {
        return orderedRods.size();
    }

    public void addToOrderedRods(OrderedRod orderedRod) {
        orderedRods.add(orderedRod);
        addToTotalPrice(orderedRod.getPrice());
    }

    private void addToTotalPrice(int price) {
        totalPrice += price;
    }

    private void sumTotalLeftovers() {
        totalLeftovers = 0;
        for(OrderedRod i: orderedRods) {
            totalLeftovers += i.getLeftovers();
        }
    }

    //Each time this method is called the leftovers are being recalculated.
    public long getTotalLeftovers() {
        sumTotalLeftovers();
        return totalLeftovers;
    }

    public long getTotalPrice() {
        return totalPrice;
    }

}
