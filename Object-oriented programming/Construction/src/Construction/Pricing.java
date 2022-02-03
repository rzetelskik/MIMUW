package Construction;

public class Pricing {
    private StockRod[] stockRods;

    public Pricing(StockRod[] stockRods) {
        this.stockRods = stockRods;
    }

    public StockRod getStockRod(int i) {
        return stockRods[i];
    }

    public int getLength() {
        return stockRods.length;
    }

}
