package Browser;
import Common.Config;
import Common.Constants;
import Common.ExceptionUtils;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.Term;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.queryparser.complexPhrase.ComplexPhraseQueryParser;
import org.apache.lucene.search.*;
import org.apache.lucene.search.highlight.*;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

// A class implementing IBrowser interface using Apache Lucene.
public class LuceneBrowser implements IBrowser {
    private BrowserSettings settings;
    private Directory directory;
    private Analyzer analyzer;

    public LuceneBrowser(Config config, BrowserSettings settings) throws IOException {
        this.settings = settings;
        directory = FSDirectory.open(config.getIndexerPath());
        analyzer = config.newCustomAnalyzer();
    }

    // Gets a string corresponding to the title field
    // and the selected language.
    private String getTitleString() {
        return (settings.getLanguage() == BrowserSettings.Language.PL)
                ? Constants.TITLE_PL : Constants.TITLE_EN;
    }

    // Gets a string corresponding to the body field
    // and the selected language.
    private String getBodyString() {
        return (settings.getLanguage() == BrowserSettings.Language.PL)
                ? Constants.BODY_PL : Constants.BODY_EN;
    }

    // Using a provided string prepares and returns a query.
    private Query getQuery(String string) throws QueryParsingException {
        try {
            Query titleQuery, bodyQuery;

            // Chooses a query matching the selected mode.
            switch (settings.getQueryType()) {
                case PHRASE:
                    titleQuery = (new ComplexPhraseQueryParser(getTitleString(), analyzer)).parse(string);
                    bodyQuery = (new ComplexPhraseQueryParser(getBodyString(), analyzer)).parse(string);
                    break;
                case FUZZY:
                    titleQuery = new FuzzyQuery(new Term(getTitleString(), string));
                    bodyQuery = new FuzzyQuery(new Term(getBodyString(), string));
                    break;
                default:
                    titleQuery = (new QueryParser(getTitleString(), analyzer)).parse(string);
                    bodyQuery = (new QueryParser(getBodyString(), analyzer)).parse(string);
                    break;
            }

            // Builds a query using the elementary queries.
            return new BooleanQuery.Builder()
                    .add(titleQuery, BooleanClause.Occur.SHOULD)
                    .add(bodyQuery, BooleanClause.Occur.SHOULD)
                    .build();
        } catch (ParseException exc) {
            throw new QueryParsingException();
        }
    }

    private Formatter newCustomFormatter() {
        String postTag = Constants.ANSI_NORMAL;
        String preTag = settings.isColorOn()
                ? Constants.ANSI_BOLD_RED : Constants.ANSI_BOLD;

        // Instead of using the default tags, uses the ANSI tags.
        return new SimpleHTMLFormatter(preTag, postTag);
    }

    private List<Document> browseIndex(Query query) throws BrowsingException {
        try (IndexReader indexReader = DirectoryReader.open(directory)) {
            List<Document> documents = new ArrayList<>();
            IndexSearcher indexSearcher = new IndexSearcher(indexReader);

            // Get documents from all top results (limited as per settings' value).
            TopDocs topDocs = indexSearcher.search(query, settings.getLimit());
            for (ScoreDoc scoreDoc: topDocs.scoreDocs) {
                documents.add(indexSearcher.doc(scoreDoc.doc));
            }

            return documents;
        } catch (IOException exc) {
            throw new BrowsingException();
        }

    }

    public List<BrowserMatch> prepareMatches(String queryString) throws QueryParsingException, BrowsingException, DetailsException {
        List<BrowserMatch> matches = new ArrayList<>();

        try {
            Query query = getQuery(queryString);
            List<Document> documents = browseIndex(query);

            QueryScorer scorer = new QueryScorer(query);
            Formatter formatter = newCustomFormatter();
            Highlighter highlighter = new Highlighter(formatter, scorer);


            for (Document doc: documents) {
                BrowserMatch match = new BrowserMatch();

                match.setPath(doc.get(Constants.PATH_FILE));

                // If the details mode is on, prepare the fragments to be displayed.
                if (settings.isDetailsOn()) {
                    String content = doc.get(getBodyString());

                    TokenStream stream = analyzer.tokenStream(getBodyString(), content);
                    try {
                        // Highlight the best fragments. Display up to three.
                        String[] fragments = highlighter.getBestFragments(stream, content, 3);
                        match.setFragments(fragments);
                    } catch (IOException | IllegalArgumentException exc) {
                        throw new DetailsException();
                    }
                }
                matches.add(match);
            }
        } catch (InvalidTokenOffsetsException exc) {
            ExceptionUtils.printDescription(exc);
        }

        return matches;
    }
}

