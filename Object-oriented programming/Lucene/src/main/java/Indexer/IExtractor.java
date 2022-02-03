package Indexer;

import java.nio.file.Path;

// Extractor interface.
public interface IExtractor {

    // Exception thrown in case of an error occurring during content's parsing.
    class ExtractorParsingException extends Exception {
        public ExtractorParsingException() {
            super("Extracted file's contents could not be parsed.");
        }
    }

    // Exception thrown in case of a file extraction error.
    class ExtractorFileException extends Exception {
        public ExtractorFileException() {
            super("File's contents could not be extracted.");
        }
    }

    // Extracts the file's content and parses it to a string. Takes the file's path as an argument.
    String parseToString(Path path) throws ExtractorParsingException, ExtractorFileException;
}
