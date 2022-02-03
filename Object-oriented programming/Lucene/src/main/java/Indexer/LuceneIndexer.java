package Indexer;

import Common.Config;
import Common.Constants;
import Common.ExceptionUtils;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.*;
import org.apache.lucene.search.*;
import org.apache.lucene.store.*;
import org.apache.tika.langdetect.OptimaizeLangDetector;
import org.apache.tika.language.detect.LanguageDetector;
import org.apache.tika.language.detect.LanguageResult;
import org.apache.lucene.document.Document;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;

// A public class implementing the IIndexer interface using Apache Lucene.
public class LuceneIndexer implements IIndexer {
    private Directory directory;
    private IExtractor extractor;
    private Analyzer analyzer;
    private LanguageDetector detector;

    public LuceneIndexer(Config config) throws IOException {
        directory = FSDirectory.open(config.getIndexerPath());
        extractor = new TikaExtractor();
        analyzer = config.newCustomAnalyzer();
        detector = new OptimaizeLangDetector().loadModels();
    }

    // Detects the language of a string's contents and returns an appropriate string.
    private String detectLanguage(String string) {
        LanguageResult result = detector.detect(string);
        return result.getLanguage();
    }

    // Creates a document corresponding to a file with a provided filepath.
    private Document createFileDoc(Path path) {
        try {
            Document document = new Document();
            document.add(new StringField(Constants.PATH_FILE, path.toString(), Field.Store.YES));

            String title, body;
            String content = extractor.parseToString(path);

            // If the detected language was polish, set the fields to polish.
            // Otherwise set the fields to english (default).
            if (detectLanguage(content).equals("pl")) {
                title = Constants.TITLE_PL;
                body = Constants.BODY_PL;
            } else {
                title = Constants.TITLE_EN;
                body = Constants.BODY_EN;
            }

            document.add(new TextField(title, path.getFileName().toString(), Field.Store.YES));
            document.add(new TextField(body, content, Field.Store.YES));

            return document;
        } catch (IExtractor.ExtractorParsingException | IExtractor.ExtractorFileException exc) {
            ExceptionUtils.printDescription(exc);
        }
        return null;
    }

    // Index a file with a provided filepath.
    private void indexFileDoc(IndexWriter writer, Path path) throws IOException {
        if (Files.isRegularFile(path)) {
            Document document = createFileDoc(path);

            if (document != null) {
                writer.addDocument(document);
            }
        }
    }

    // Remove all file documents with a provided path from the index.
    private void removeFileDoc(IndexWriter writer, Path path) throws IOException {
        // Create a term to look for only based on the path.
        Term term = new Term(Constants.PATH_FILE, path.toString());

        writer.deleteDocuments(term);
        writer.commit();
    }

    // Create a document corresponding to a directory with a provided path.
    private Document createDirectoryDoc(Path path) {
        Document document = new Document();

        document.add(new StringField(Constants.PATH_DIR, path.toString(), Field.Store.YES));

        return document;
    }

    // Index a directory with a provided path.
    public void indexDirectoryDoc(Path path) throws IOException {
        IndexWriterConfig indexWriterConfig = new IndexWriterConfig(analyzer);

        try (IndexWriter writer = new IndexWriter(directory, indexWriterConfig)) {
            Document document = createDirectoryDoc(path);

            writer.addDocument(document);
            writer.commit();
        }
    }

    // Remove all directory documents with a provided path from the index.
    private void removeDirectoryDoc(IndexWriter writer, Path path) throws IOException {
        // Create a term to look for only based on the path.
        Term term = new Term(Constants.PATH_DIR, path.toString());

        writer.deleteDocuments(term);
        writer.commit();
    }

    public void indexDocs(Path path, boolean updateDocs) throws IOException {
        IndexWriterConfig indexWriterConfig = new IndexWriterConfig(analyzer);

        try (IndexWriter writer = new IndexWriter(directory, indexWriterConfig)) {
            // If the path corresponds to a directory, walk a file tree and
            // index all files inside of it.
            if (Files.isDirectory(path)) {
                Files.walkFileTree(path, new SimpleFileVisitor<Path>() {
                    @Override
                    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                        try {
                            if (updateDocs) {
                                // Looks for matching documents and deletes them first.
                                removeFileDoc(writer, file);
                            }
                            indexFileDoc(writer, file);
                        } catch (IOException exc) {
                            // Do not index files that cannot be read.
                        }

                        return FileVisitResult.CONTINUE;
                    }
                });
            } else {
                // If the path corresponds to a file, only index this file.
                if (updateDocs) {
                    removeFileDoc(writer, path);
                }
                indexFileDoc(writer, path);
            }
        }
    }

    public void removeDocs(Path path) throws IOException {
        IndexWriterConfig indexWriterConfig = new IndexWriterConfig(analyzer);

        try (IndexWriter writer = new IndexWriter(directory, indexWriterConfig)) {
            // In case the path leads to a file - remove file with a specified path.
            Query fileQuery = new TermQuery(new Term(Constants.PATH_FILE, path.toString()));
            writer.deleteDocuments(fileQuery);

            // In case it leads to a directory - remove the directory and all its contents.
            removeDirectoryDoc(writer, path);
            Query dirQuery = new WildcardQuery(new Term(Constants.PATH_FILE, path.toString() + "/*"));
            writer.deleteDocuments(dirQuery);

            writer.commit();
        }
    }

    public List<Path> getDirectoryList() {
        List<Path> list = new ArrayList<>();

        try (IndexReader reader = DirectoryReader.open(directory)) {
            //Look for all documents that have a PATH_DIR field.
            Query query = new WildcardQuery(new Term(Constants.PATH_DIR, "*"));
            IndexSearcher searcher = new IndexSearcher(reader);


            TopDocs topDocs = searcher.search(query, Integer.MAX_VALUE);
            for (ScoreDoc scoreDoc: topDocs.scoreDocs) {
                Document doc = (searcher.doc(scoreDoc.doc));
                Path path = Paths.get(doc.getField(Constants.PATH_DIR).stringValue());
                list.add(path);
            }
        } catch (IOException exc) {
            //Ignore and return empty list.
        }

        return list;
    }

    public void purge() throws IOException {
        IndexWriterConfig indexWriterConfig = new IndexWriterConfig(analyzer);

        try (IndexWriter writer = new IndexWriter(directory, indexWriterConfig)) {
            writer.deleteAll();
            writer.commit();
        }
    }
}

