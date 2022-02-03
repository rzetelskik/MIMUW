package Browser;

// A class used to store a browsing result.
public class BrowserMatch {
    private String path;
    private String[] fragments;

    public BrowserMatch() {}

    public void setPath(String path) {
        this.path = path;
    }

    public void setFragments(String[] fragments) {
        this.fragments = fragments;
    }

    public String getPath() {
        return path;
    }

    public String[] getFragments() {
        return fragments;
    }

    public boolean isFragmentsEmpty() {
        return (fragments == null);
    }
}
