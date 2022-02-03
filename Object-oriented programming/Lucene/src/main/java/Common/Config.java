package Common;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.en.EnglishAnalyzer;
import org.apache.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper;
import org.apache.lucene.analysis.pl.PolishAnalyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

// A class shared between the packages defining the common configuration.
public class Config {
    private final Path indexerPath = Paths.get(System.getProperty("user.home"), ".index");

    public Path getIndexerPath() {
        return indexerPath;
    }

    public PerFieldAnalyzerWrapper newCustomAnalyzer() {
        Map<String, Analyzer> analyzerMap = new HashMap<>();
        EnglishAnalyzer englishAnalyzer = new EnglishAnalyzer();
        PolishAnalyzer polishAnalyzer = new PolishAnalyzer();

        analyzerMap.put(Constants.TITLE_EN, englishAnalyzer);
        analyzerMap.put(Constants.BODY_EN, englishAnalyzer);
        analyzerMap.put(Constants.TITLE_PL, polishAnalyzer);
        analyzerMap.put(Constants.BODY_PL, polishAnalyzer);

        return new PerFieldAnalyzerWrapper(new StandardAnalyzer(), analyzerMap);
    }

}
