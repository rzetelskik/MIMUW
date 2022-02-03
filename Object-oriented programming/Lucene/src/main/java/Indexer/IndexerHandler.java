package Indexer;

import Common.Config;
import Common.ExceptionUtils;
import org.slf4j.bridge.SLF4JBridgeHandler;

import java.io.IOException;
import java.nio.file.*;
import java.util.List;

// A public class used to handle the indexer.
public class IndexerHandler {
    private final IIndexer indexer;
    private final Config config;

    private IndexerHandler(IIndexer indexer, Config config) {
        this.indexer = indexer;
        this.config = config;
    }

    // Add a directory with a provided path
    // and all of its contents to the index.
    private void addDirectory(Path path) {
        try {
            indexer.indexDirectoryDoc(path);
            // Create new documents (do not try to update).
            indexer.indexDocs(path, false);
        } catch (IOException exc) {
            ExceptionUtils.printDescription(exc);
        }
    }

    // Remove a directory with a provided path
    // and all of its contents from the index.
    private void removeDirectory(Path path) {
        try {
            indexer.removeDocs(path);
        } catch (IOException exc) {
            ExceptionUtils.printDescription(exc);
        }
    }

    // Clear the index.
    private void purge() {
        try {
            indexer.purge();
        } catch (IOException exc) {
            ExceptionUtils.printDescription(exc);
        }
    }

    private void reindex() {
        try {
            // Get the list of indexed directories.
            List<Path> list = indexer.getDirectoryList();
            // Clear the index.
            indexer.purge();

            // Index the directories and their contents.
            for (Path path: list) {
                addDirectory(path);
            }
        } catch (IOException exc) {
            ExceptionUtils.printDescription(exc);
        }
    }

    private void list(OutputHandler outputHandler) {
        try {
            // Get the list of indexed directories.
            List<Path> list = indexer.getDirectoryList();

            // Print the directories' paths to the output.
            for (Path path: list) {
                outputHandler.printCanonicalPath(path);
            }
        } catch (IOException exc) {
            ExceptionUtils.printDescription(exc);
        }
    }


    public static void main(String[] args) {
        SLF4JBridgeHandler.removeHandlersForRootLogger();
        SLF4JBridgeHandler.install();

        try {
            final Config config = new Config();
            final IIndexer indexer = new LuceneIndexer(config);
            final IndexerHandler indexerHandler = new IndexerHandler(indexer, config);

            ParameterParser parameterParser = new ParameterParser(args);

            switch(parameterParser.getRequestType()) {
                case ADD:
                    indexerHandler.addDirectory(parameterParser.getDir());
                    break;
                case REMOVE:
                    indexerHandler.removeDirectory(parameterParser.getDir());
                    break;
                case PURGE:
                    indexerHandler.purge();
                    break;
                case REINDEX:
                    indexerHandler.reindex();
                    break;
                case LIST:
                    OutputHandler outputHandler = new OutputHandler();
                    indexerHandler.list(outputHandler);
                    break;
                default:
                    IndexerWatch indexerWatch = new IndexerWatch(indexer);
                    indexerWatch.processEvents();
                    break;
            }
        } catch (IOException | ParameterParser.WrongNumberOfParameters
        | ParameterParser.WrongParameter exc) {
            ExceptionUtils.printDescription(exc);
        }
    }
}
