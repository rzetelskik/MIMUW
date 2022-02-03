package Construction;

import java.util.ArrayList;

public class OrderedRod {
    private StockRod stockRod;
    private ArrayList<Integer> sections;
    private int leftovers;

    public OrderedRod(StockRod stockRod) {
        this.stockRod = stockRod;
        this.leftovers = stockRod.getSize();
        sections = new ArrayList<>();
    }

    //Section is added to the array and subtracted from leftovers.
    public void cutSection(int section) {
        leftovers -= section;
        sections.add(section);
    }

    public int getSize() {
        return stockRod.getSize();
    }

    public int getPrice() {
        return stockRod.getPrice();
    }

    public int getLeftovers() {
        return leftovers;
    }

    public int getSection(int i) {
        return sections.get(i);
    }

    public int getSectionsLength() {
        return sections.size();
    }
}
