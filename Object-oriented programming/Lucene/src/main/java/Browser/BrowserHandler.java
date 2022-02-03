package Browser;

import Common.Config;
import Common.ExceptionUtils;

import org.jline.reader.EndOfFileException;
import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.reader.UserInterruptException;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;


import java.io.IOException;
import java.util.List;

// A public class used to handle the browser and its settings.
public class BrowserHandler {
    private IBrowser browser;
    private BrowserSettings settings;

    private BrowserHandler(IBrowser browser, BrowserSettings settings) {
        this.browser = browser;
        this.settings = settings;
    }

    private void processLangRequest(String string) {
        switch(string) {
            case "en":
                settings.setLanguage(BrowserSettings.Language.EN);
                break;
            case "pl":
                settings.setLanguage(BrowserSettings.Language.PL);
                break;
        }
    }

    private void processDetailsRequest(String string) {
        switch(string) {
            case "on":
                settings.setDetails(BrowserSettings.State.ON);
                break;
            case "off":
                settings.setDetails(BrowserSettings.State.OFF);
                break;
        }
    }

    private void processLimitRequest(Integer integer) {
        settings.setLimit(integer);
    }

    private void processQueryTypeRequest(InputParser.RequestType type) {
        switch (type) {
            case TERM:
                settings.setQueryType(BrowserSettings.QueryType.TERM);
                break;
            case PHRASE:
                settings.setQueryType(BrowserSettings.QueryType.PHRASE);
                break;
            case FUZZY:
                settings.setQueryType(BrowserSettings.QueryType.FUZZY);
                break;
        }
    }

    private void processColorRequest(String string) {
        switch(string) {
            case "on":
                settings.setColor(BrowserSettings.State.ON);
                break;
            case "off":
                settings.setColor(BrowserSettings.State.OFF);
                break;
        }
    }

    private void processQuery(String string, OutputHandler outputHandler) {
        try {
            // Get the list of best browsing results.
            List<BrowserMatch> list = browser.prepareMatches(string);

            outputHandler.printOutput(list, settings.isDetailsOn());
        } catch (IBrowser.QueryParsingException | IBrowser.BrowsingException |
                IBrowser.DetailsException exc) {
            ExceptionUtils.printDescription(exc);
        }
    }

    // Process the request according to its type.
    private void process(InputParser parser, OutputHandler outputHandler) {
        try {
            switch(parser.getRequestType()) {
                case LANG:
                    processLangRequest(parser.getString());
                    break;
                case DETAILS:
                    processDetailsRequest(parser.getString());
                    break;
                case LIMIT:
                    processLimitRequest(parser.getInteger());
                    break;
                case COLOR:
                    processColorRequest(parser.getString());
                    break;
                case TERM:
                case PHRASE:
                case FUZZY:
                    processQueryTypeRequest(parser.getRequestType());
                    break;
                case QUERY:
                    processQuery(parser.getString(), outputHandler);
                    break;
            }
        } catch (Exception exc) {
            ExceptionUtils.printDescription(exc);
         }

    }

    public static void main(String[] args) {
        // Builds a new terminal and initiates
        // a Read-Eval-Print-Loop interpreter.
        try (Terminal terminal = TerminalBuilder.builder()
                .dumb(true)
                .jna(false)
                .jansi(true)
                .build()) {
            LineReader lineReader = LineReaderBuilder.builder()
                    .terminal(terminal)
                    .build();

            BrowserSettings settings = new BrowserSettings();
            Config config = new Config();
            IBrowser browser = new LuceneBrowser(config, settings);
            BrowserHandler browserHandler = new BrowserHandler(browser, settings);
            OutputHandler outputHandler = new OutputHandler(terminal);

            // Loops until the program has been terminated
            // or an error occurred.
            while (true) {
                String line;
                // Reads a line, parses it and processes it.
                try {
                    line = lineReader.readLine("> ").trim();

                    InputParser parser = new InputParser();
                    parser.parse(line);

                    browserHandler.process(parser, outputHandler);

                } catch (UserInterruptException | EndOfFileException  exc) {
                    break;
                } catch (InputParser.IllicitQuery | InputParser.WrongControlCommandArguments |
                        InputParser.IllicitControlCommand exc) {
                    // Does not break the loop.
                    ExceptionUtils.printDescription(exc);
                }
            }
        } catch (IOException exc) {
            ExceptionUtils.printDescription(exc);
        }

    }

}
