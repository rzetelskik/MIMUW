package Construction;

import java.util.Scanner;

public class Stdin implements IInputProvider {
    private Scanner scanner;

    public Stdin() {
        scanner = new Scanner(System.in);
    }

    public int nextInt() {
        return scanner.nextInt();
    }

    public String nextLine() {
        return scanner.nextLine();
    }
}
