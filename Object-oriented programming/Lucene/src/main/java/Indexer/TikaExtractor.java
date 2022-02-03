package Indexer;

import org.apache.tika.Tika;
import org.apache.tika.exception.TikaException;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

// A public class implementing the IExtractor interface using Apache Tika.
public class TikaExtractor implements IExtractor {
    private Tika tika;

    public TikaExtractor() {
        tika = new Tika();
    }

    public String parseToString(Path path) throws ExtractorParsingException, ExtractorFileException {
        try (InputStream stream = Files.newInputStream(path)) {
            return tika.parseToString(stream);
        } catch (TikaException exc) {
            throw new ExtractorParsingException();
        } catch (IOException exc) {
            throw new ExtractorFileException();
        }
    }
}
