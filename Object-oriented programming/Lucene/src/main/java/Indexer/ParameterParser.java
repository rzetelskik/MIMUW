package Indexer;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static java.nio.file.LinkOption.NOFOLLOW_LINKS;

// A public class used to parse the command line parameters.
public class ParameterParser {
    private RequestType requestType;
    private Path dir;

    // Exception thrown in case of an unaccepted number of parameters for certain request.
    public class WrongNumberOfParameters extends Exception {
        public WrongNumberOfParameters() {
            super("Wrong number of parameters.");
        }
    }

    // Exception thrown in case of an unaccepted parameter.
    public class WrongParameter extends Exception {
        public WrongParameter() {
            super("Wrong parameter.");
        }
    }

    // Enum specifying all kinds of accepted requests.
    public enum RequestType {
        ADD, REMOVE, PURGE, REINDEX, LIST, NONE
    }

    // A class' constructor. Takes command line parameters as arguments and parses them, setting
    // the attributes accordingly.
    public ParameterParser(String[] args) throws WrongParameter, WrongNumberOfParameters {
        if (args.length == 0 ) {
            requestType = RequestType.NONE;
        } else if (args.length > 2) {
            throw new WrongNumberOfParameters();
        } else {
            requestType = checkRequestType(args[0]);

            switch (requestType) {
                case ADD:
                case REMOVE:
                    // ADD and REMOVE commands require one extra parameter - a directory path.
                    if (args.length == 2) {
                        if (Files.isDirectory(Paths.get(args[1]), NOFOLLOW_LINKS)) {
                            dir = Paths.get(args[1]).toAbsolutePath();
                            break;
                        } else {
                            throw new WrongParameter();
                        }
                    } else {
                        throw new WrongNumberOfParameters();
                    }
                default:
                    // All the other requests do not allow any other parameters.
                    if (args.length != 1) {
                        throw new WrongNumberOfParameters();
                    }
            }
        }
    }

    public RequestType getRequestType() {
        return requestType;
    }

    public Path getDir() {
        // Only returns a feasible path for ADD or REMOVE requestType.
        // Otherwise returns null.
        return dir;
    }

    //
    private RequestType checkRequestType(String arg) throws WrongParameter {
        // Switch uses equals method to compare the two strings.
        switch (arg) {
            case "--add":
                return RequestType.ADD;
            case "--rm":
                return RequestType.REMOVE;
            case "--purge":
                return RequestType.PURGE;
            case "--reindex":
                return RequestType.REINDEX;
            case "--list":
                return RequestType.LIST;
            default:
                // If the parameter is none of the above, throws an exception.
                throw new WrongParameter();
        }
    }
}
