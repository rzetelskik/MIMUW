package petrinet;

import java.util.*;
import java.util.concurrent.Semaphore;

public class PetriNet<T> {
    private Map<T, Integer> currentMarking;
    private Semaphore mutex;
    private Deque<ThreadPair<T>> threadQueue;

    // Class used for queueing paused threads that have requested a fire method.
    private static class ThreadPair<T> {
        private Collection<Transition<T>> transitions;
        private Semaphore threadSemaphore;

        private ThreadPair(Collection<Transition<T>> transitions, Semaphore threadSemaphore) {
            this.transitions = transitions;
            this.threadSemaphore = threadSemaphore;
        }
    }

    public PetriNet(Map<T, Integer> initial, boolean fair) {
        currentMarking = new HashMap<>(initial);
        mutex = new Semaphore(1, fair);
        threadQueue = new ArrayDeque<>();
    }

    public Integer getTokens(T place) {
        return currentMarking.getOrDefault(place, 0);
    }

    public Set<Map<T, Integer>> reachable(Collection<Transition<T>> transitions) {
        Set<Map<T, Integer>> markingSet = new HashSet<>();
        Deque<Map<T, Integer>> stack = new ArrayDeque<>();

        try {
            mutex.acquire();

            stack.push(currentMarking);
            while (!stack.isEmpty()) {
                Map<T, Integer> marking = stack.pop();
                if (markingSet.contains(marking)) continue;

                markingSet.add(marking);
                for (Transition<T> transition : transitions) {
                    if (!transition.isEnabled(marking)) continue;
                    stack.push(transition.fire(marking));
                }
            }

            return markingSet;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            System.err.println(e.getMessage());

            return Collections.emptySet();
        } finally {
            mutex.release();
        }
    }

    private Optional<Transition<T>> getAnyEnabledTransition(Collection<Transition<T>> transitions) {
        for (Transition<T> t : transitions) {
            if (t.isEnabled(currentMarking)) return Optional.of(t);
        }
        return Optional.empty();
    }

    private Optional<Semaphore> getEnabledSemaphoreFromQueue() {
        Iterator<ThreadPair<T>> it = threadQueue.iterator();
        while (it.hasNext()) {
            ThreadPair<T> pair = it.next();

            if (getAnyEnabledTransition(pair.transitions).isPresent()) {
                it.remove();
                return Optional.of(pair.threadSemaphore);
            }
        }
        return Optional.empty();
    }

    public Transition<T> fire(Collection<Transition<T>> transitions) throws InterruptedException {
        boolean priorityPassed = false;
        try {
            mutex.acquire();

            Optional<Transition<T>> enabled = getAnyEnabledTransition(transitions);
            if (enabled.isEmpty()) {
                Semaphore ownSemaphore = new Semaphore(0);
                threadQueue.addLast(new ThreadPair<>(transitions, ownSemaphore));
                mutex.release();
                ownSemaphore.acquire();
                enabled = getAnyEnabledTransition(transitions);
            }

            assert(enabled.isPresent());
            currentMarking = enabled.get().fire(currentMarking);

            Optional<Semaphore> queued = getEnabledSemaphoreFromQueue();
            if (queued.isPresent()) {
                priorityPassed = true;
                // Pass priority to the selected thread.
                queued.get().release();
            }

            return enabled.get();
        } finally {
            if (!priorityPassed) mutex.release();
        }
    }
}