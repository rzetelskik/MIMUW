package Indexer;

import Common.ExceptionUtils;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.nio.file.LinkOption.NOFOLLOW_LINKS;
import static java.nio.file.StandardWatchEventKinds.*;

public class IndexerWatch {
    private final IIndexer indexer;
    private final WatchService watcher;
    private final Map<WatchKey, Path> keys;

    @SuppressWarnings("unchecked")
    static <T> WatchEvent<T> cast(WatchEvent<?> event) {
        return (WatchEvent<T>) event;
    }

    // Creates a WatchService and registers all the directories
    // in the directory list.
    public IndexerWatch(IIndexer indexer) throws IOException {
        this.indexer = indexer;
        this.watcher = FileSystems.getDefault().newWatchService();
        this.keys = new HashMap<>();

        List<Path> list = indexer.getDirectoryList();

        for (Path path: list) {
            registerAll(path);
        }
    }

    // Register the given directory with the WatchService.
    private void register(Path dir) throws IOException {
        WatchKey key = dir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY);
        keys.put(key, dir);
    }


    // Register the given directory, and all its sub-directories,
    // with the WatchService.
    private void registerAll(final Path start) throws IOException {
        // Register directory and sub-directories.
        Files.walkFileTree(start, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                register(dir);
                return FileVisitResult.CONTINUE;
            }
        });
    }

    // Process all events for keys queued to the watcher.
    public void processEvents() {
        while (true) {
            // Wait for key to be signalled.
            WatchKey key;
            try {
                key = watcher.take();
            } catch (InterruptedException x) {
                return;
            }

            Path dir = keys.get(key);
            if (dir == null) {
                continue;
            }

            for (WatchEvent<?> event : key.pollEvents()) {
                WatchEvent.Kind<?> kind = event.kind();

                if (kind == OVERFLOW) {
                    // Ignore overflow event.
                    continue;
                }

                // Context for directory entry event is the file name of entry.
                WatchEvent<Path> ev = cast(event);
                Path name = ev.context();
                Path child = dir.resolve(name).toAbsolutePath();

                // If directory is created, then
                // register it and its sub-directories.
                if (kind == ENTRY_CREATE) {
                    try {
                        if (Files.isDirectory(child, NOFOLLOW_LINKS)) {
                            registerAll(child);
                        }
                        indexer.indexDocs(child, false);
                    } catch (IOException exc) {
                        ExceptionUtils.printDescription(exc);
                    }
                } else if (kind == ENTRY_MODIFY) {
                    try {
                        indexer.indexDocs(child, true);
                    } catch (IOException exc){
                        ExceptionUtils.printDescription(exc);
                    }
                } else if (kind == ENTRY_DELETE) {
                    try {
                        indexer.removeDocs(child);
                    } catch (IOException exc) {
                        ExceptionUtils.printDescription(exc);
                    }
                }
            }

            // Reset key and remove from set if directory is no longer accessible.
            boolean valid = key.reset();
            if (!valid) {
                keys.remove(key);

                // If all directories are inaccessible.
                if (keys.isEmpty()) {
                    break;
                }
            }
        }
    }

}
