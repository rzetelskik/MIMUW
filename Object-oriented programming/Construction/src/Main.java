import Construction.*;

public class Main {

    public static void main(String[] args) {
        IInputHandler inputHandler = new InputHandler(new Stdin(), new StrategyFactory());
        Project project = inputHandler.getInput();
        Order order = project.getStrategy().processRequest(project);
        IOutputHandler outputHandler = new OutputHandler();

        outputHandler.handleOutput(order);
    }
}
