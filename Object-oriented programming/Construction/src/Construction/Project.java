package Construction;

public class Project {
    private Pricing pricing;
    private Integer[] request;
    private IStrategy strategy;

    public Project(Pricing pricing, Integer[] request, IStrategy strategy) {
        this.pricing = pricing;
        this.request = request;
        this.strategy = strategy;
    }

    public Integer getRequest(int i) {
        return request[i];
    }

    public int getRequestLength() {
        return request.length;
    }

    public StockRod getPricingRod(int i) {
        return pricing.getStockRod(i);
    }

    public int getPricingLength() {
        return pricing.getLength();
    }

    public IStrategy getStrategy() {
        return strategy;
    }
}
