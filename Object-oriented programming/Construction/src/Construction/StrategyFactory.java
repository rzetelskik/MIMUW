package Construction;

public class StrategyFactory implements IStrategyFactory {

    public StrategyFactory() {}

    public IStrategy getStrategy(String string) {
        if (string.equals("minimalistyczna")) {
            return new Minimal();
        } else if (string.equals("maksymalistyczna")) {
            return new Maximal();
        } else if (string.equals("ekonomiczna")) {
            return new Economic();
        } else {
            return new Ecological();
        }

    }
}
