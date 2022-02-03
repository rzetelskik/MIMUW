package Common;

// Shared class used for exceptions' handling.
// All communicates are printed to the standard diagnostic stream.
public class ExceptionUtils {

    // Print the class of the exception.
    private static void printErrClass(Exception exc) {
        System.err.println("Exception caught in: " + exc.getClass());
    }

    // Print the message of the exception.
    private static void printErrMessage(Exception exc) {
        System.err.println("Error message: " + exc.getMessage());
    }

    public static void printDescription(Exception exc) {
        printErrClass(exc);
        printErrMessage(exc);
    }
}
