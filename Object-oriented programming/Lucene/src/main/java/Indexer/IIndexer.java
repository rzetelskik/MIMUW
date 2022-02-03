package Indexer;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

// Interface used to index files.
public interface IIndexer {
    // Adds a file with a provided path to the index.
    // If updateDocs is true, it updates the file in the index (if it was already added).
    void indexDocs(Path path, boolean updateDocs) throws IOException;

    // Creates a distinct index entry to store a directory's path.
    void indexDirectoryDoc(Path path) throws IOException;

    // Removes a file if the path is of a file.
    // If the path is of a directory, removes all files from a directory
    // and the directory itself.
    void removeDocs(Path path) throws IOException;

    // Prepares and returns a list of paths of all indexed directories.
    List<Path> getDirectoryList() throws IOException;

    // Deletes all entries in the index.
    // Does not delete the index itself.
    void purge() throws IOException;
}
