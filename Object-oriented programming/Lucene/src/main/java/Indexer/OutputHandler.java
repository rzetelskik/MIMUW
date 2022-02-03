package Indexer;

import java.io.IOException;
import java.nio.file.Path;

// Output class used for printing to standard output.
public class OutputHandler {

    public OutputHandler() {}

    // Takes a Path as an argument and prints it out in a canonical form.
    public void printCanonicalPath(Path path) throws IOException {
        String canonicalPath = path.toFile().getCanonicalPath();
        System.out.println(canonicalPath);
    }
}
