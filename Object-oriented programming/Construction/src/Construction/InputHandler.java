package Construction;

public class InputHandler implements IInputHandler {
    private IInputProvider inputProvider;
    private IStrategyFactory strategyFactory;

    public InputHandler(IInputProvider inputProvider, IStrategyFactory strategyFactory) {
        this.inputProvider = inputProvider;
        this.strategyFactory = strategyFactory;
    }

    private int getArraySize() {
        int size;
        size = inputProvider.nextInt();
        inputProvider.nextLine();

        return size;
    }

    private StockRod getRod() {
        int price, size;
        size = inputProvider.nextInt();
        price = inputProvider.nextInt();
        inputProvider.nextLine();

        return new StockRod(price, size);
    }

    private StockRod[] getRodArray() {
        int size = getArraySize();
        StockRod[] stockRodArray = new StockRod[size];

        for (int i = 0; i < size; i++) {
            stockRodArray[i] = getRod();
        }

        return stockRodArray;
    }

    private Integer[] getIntArray() {
        int size = getArraySize();
        Integer[] intArray = new Integer[size];

        for (int i = 0; i < size; i++) {
            intArray[i] = inputProvider.nextInt();
        }
        if (size > 0) {
            inputProvider.nextLine();
        }

        return intArray;
    }

    private IStrategy getStrategy() {
        String string = inputProvider.nextLine();

        return strategyFactory.getStrategy(string);
    }

    public Project getInput() {
        Pricing pricing = new Pricing(getRodArray());
        Integer[] request = getIntArray();
        IStrategy strategy = getStrategy();

        return new Project(pricing, request, strategy);
    }
}