package Browser;

// A class used to parse the provided input.
public class InputParser {
    private RequestType requestType;
    private Integer integer;
    private String string;

    public InputParser() {}

    public class IllicitQuery extends Exception {
        public IllicitQuery() {
            super("Illicit query.");
        }
    }

    public class IllicitControlCommand extends Exception {
        public IllicitControlCommand() {
            super("Illicit control command.");
        }
    }

    public class WrongControlCommandArguments extends Exception {
        public WrongControlCommandArguments() {
            super("Wrong control command argument.");
        }
    }

    // Enum specifying all kinds of accepted commands.
    public enum RequestType {
        LANG, DETAILS, LIMIT, COLOR, TERM,
        PHRASE, FUZZY, QUERY
    }

    public RequestType getRequestType() {
        return requestType;
    }

    public String getString() {
        return string;
    }

    public Integer getInteger() {
        return integer;
    }

    private RequestType checkCommandType(String arg) throws IllicitControlCommand {
        switch (arg) {
            case "%lang":
                return RequestType.LANG;
            case "%details":
                return RequestType.DETAILS;
            case "%limit":
                return RequestType.LIMIT;
            case "%color":
                return RequestType.COLOR;
            case "%term":
                return RequestType.TERM;
            case "%phrase":
                return RequestType.PHRASE;
            case "%fuzzy":
                return RequestType.FUZZY;
            default:
                // If the command does not match
                // any of the accepted ones, throws
                // an exception.
                throw new IllicitControlCommand();
        }
    }

    private String parseLangRequest(String[] args) throws WrongControlCommandArguments {
        if (args.length == 2) {
            switch (args[1]) {
                case "pl":
                case "en":
                    return args[1];
                default:
                    // Breaks and throws an exception.
                    break;
            }
        }
        throw new WrongControlCommandArguments();
    }

    private String parseOnOffRequest(String[] args) throws WrongControlCommandArguments {
        if (args.length == 2) {
            switch (args[1]) {
                case "on":
                case "off":
                    return args[1];
                default:
                    // Breaks and throws an exception.
                    break;
            }
        }
        throw new WrongControlCommandArguments();
    }

    private Integer parseLimitRequest(String[] args) throws WrongControlCommandArguments {
        if (args.length == 2) {
            try {
                Integer num = Integer.parseInt(args[1]);
                if (num >= 0) {
                    return num;
                }
            } catch (NumberFormatException exc) {
                throw new WrongControlCommandArguments();
            }
        }
        throw new WrongControlCommandArguments();
    }


    public void parse(String line) throws IllicitQuery,
            IllicitControlCommand, WrongControlCommandArguments {
        if (line.length() > 0) {
            if (line.charAt(0) == '%') {
                // Gets the arguments by splitting the line on spaces.
                String[] args = line.trim().split("\\s+");

                // Gets the request type and proceeds accordingly.
                requestType = checkCommandType(args[0]);

                switch (requestType) {
                    case LANG:
                        string = parseLangRequest(args);
                        break;
                    case DETAILS:
                    case COLOR:
                        string = parseOnOffRequest(args);
                        break;
                    case LIMIT:
                        integer = parseLimitRequest(args);
                        break;
                    default:
                        break;
                }
            } else {
                requestType = RequestType.QUERY;
                string = line;
            }
        } else {
            throw new IllicitQuery();
        }
    }
}
