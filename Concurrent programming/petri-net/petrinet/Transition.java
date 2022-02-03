package petrinet;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

public class Transition<T> {
    private Map<T, Integer> input;
    private Collection<T> reset;
    private Collection<T> inhibitor;
    private Map<T, Integer> output;

    public Transition(Map<T, Integer> input, Collection<T> reset, Collection<T> inhibitor, Map<T, Integer> output) {
        this.input = new HashMap<>(input);
        this.reset = new HashSet<>(reset);
        this.inhibitor = new HashSet<>(inhibitor);
        this.output = new HashMap<>(output);
    }

    private Integer getTokens(T place, Map<T, Integer> marking) {
        return marking.getOrDefault(place, 0);
    }

    private boolean areInputArcsValid(Map<T, Integer> marking) {
        for (Map.Entry<T, Integer> entry: input.entrySet()) {
            if (getTokens(entry.getKey(), marking) < entry.getValue()) return false;
        }

        return true;
    }

    private boolean areInhibitorArcsValid(Map<T, Integer> marking) {
        for (T place: inhibitor) {
            if (getTokens(place, marking) != 0) return false;
        }

        return true;
    }

    public boolean isEnabled(Map<T, Integer> marking) {
        return areInputArcsValid(marking) && areInhibitorArcsValid(marking);
    }

    private void resetTokens(Map<T, Integer> marking) {
        for (T place: reset) {
            marking.remove(place);
        }
    }

    private void decreaseInputTokens(Map<T, Integer> marking) {
        for (Map.Entry<T, Integer> entry: input.entrySet()) {
            int tokens = getTokens(entry.getKey(), marking) - entry.getValue();
            if (tokens == 0) {
                marking.remove(entry.getKey());
            } else {
                marking.put(entry.getKey(), tokens);
            }
        }
    }

    private void increaseOutputTokens(Map<T, Integer> marking) {
        for (Map.Entry<T, Integer> entry: output.entrySet()) {
            int tokens = getTokens(entry.getKey(), marking) + entry.getValue();
            marking.put(entry.getKey(), tokens);
        }
    }

    public Map<T, Integer> fire(Map<T, Integer> marking) {
        Map<T, Integer> newMarking = new HashMap<>(marking);

        decreaseInputTokens(newMarking);
        resetTokens(newMarking);
        increaseOutputTokens(newMarking);

        return newMarking;
    }
}