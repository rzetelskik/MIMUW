package Browser;

import java.util.List;

// Interface of the generic browser.
public interface IBrowser {
    // Prepare all the entries matching a provided query and list them.
    List<BrowserMatch> prepareMatches(String queryString) throws QueryParsingException, BrowsingException, DetailsException;

    class QueryParsingException extends Exception {
        public QueryParsingException() {
            super("Query could not be parsed.");
        }
    }

    class BrowsingException extends Exception {
        public BrowsingException() {
            super("Error occurred while browsing the index.");
        }
    }

    class DetailsException extends Exception {
        public DetailsException() {
            super("Error occurred while preparing details to be displayed.");
        }
    }
}
