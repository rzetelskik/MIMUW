package Browser;

// A class used to save the browser's settings.
public class BrowserSettings {
    private Language language;
    private State details;
    private Integer limit;
    private State color;
    private QueryType queryType;

    // Set to default settings.
    public BrowserSettings() {
        language = Language.EN;
        details = State.OFF;
        limit = Integer.MAX_VALUE;
        color = State.OFF;
        queryType = QueryType.TERM;
    }

    // Enum specifying allowed languages.
    public enum Language {
        PL, EN
    }

    public enum State {
        ON, OFF
    }

    // Enum specifying allowed query types.
    public enum QueryType {
        TERM, PHRASE, FUZZY
    }

    public void setLanguage(Language language) {
        this.language = language;
    }

    public Language getLanguage() {
        return language;
    }

    public void setDetails(State details) {
        this.details = details;
    }

    public void setLimit(Integer limit) {
        if (limit == 0) {
            this.limit = Integer.MAX_VALUE;
        } else {
            this.limit = limit;
        }
    }

    public Integer getLimit() {
        return limit;
    }

    public void setColor(State color) {
        this.color = color;
    }

    public void setQueryType(QueryType queryType) {
        this.queryType = queryType;
    }

    public QueryType getQueryType() {
        return queryType;
    }

    public boolean isDetailsOn() {
        return (details == State.ON);
    }

    public boolean isColorOn() {
        return (color == State.ON);
    }
}
