package Browser;

import Common.Constants;
import org.jline.terminal.Terminal;

import java.util.List;

// Output class used for printing to the provided terminal.
public class OutputHandler {
    private Terminal terminal;

    public OutputHandler(Terminal terminal) {
        this.terminal = terminal;
    }


    private void printFileCount(Integer count) {
        // Bold the number.
        terminal.writer().println("File count: " + Constants.ANSI_BOLD + count + Constants.ANSI_NORMAL);
    }

    private void printMatches(List<BrowserMatch> list, boolean isDetailsOn) {
        for(BrowserMatch match: list) {
            String path = match.getPath();
            if (isDetailsOn) path += ':';

            // Bold the match's path.
            terminal.writer().println(Constants.ANSI_BOLD + path + Constants.ANSI_NORMAL);

            // Only print the fragments if details mode is ON
            // and if there were any fragments prepared.
            if (isDetailsOn && !match.isFragmentsEmpty()) {
                for (String fragment: match.getFragments()) {
                    terminal.writer().println(fragment);
                }
            }
        }
    }

    public void printOutput(List<BrowserMatch> list, boolean isDetailsOn) {
        printFileCount(list.size());
        printMatches(list, isDetailsOn);
    }
}
