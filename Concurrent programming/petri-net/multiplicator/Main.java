package multiplicator;

import petrinet.PetriNet;
import petrinet.Transition;

import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Main {
    private static final Integer THREADS = 4;
    private static final Integer TOKENS = 1;
    private PetriNet<Place> petriNet;
    private Collection<Transition<Place>> transitions;
    private Collection<Transition<Place>> finalTransition;

    private enum Place {
        A, B, INNER1, INNER2, B_MIRROR, RESULT
    }

    private Main(int val1, int val2)   {
        petriNet = new PetriNet<>(constructPlaces(val1, val2), true);
        transitions = constructTransitions();
        finalTransition = constructFinalTransition();
    }

    private Map<Place, Integer> constructPlaces(int val1, int val2) {
        return Map.of(Place.A, val1, Place.B, val2, Place.INNER1, TOKENS);
    }

    private Collection<Transition<Place>> constructTransitions() {
        Collection<Transition<Place>> transitions = new HashSet<>(4);

        transitions.add(new Transition<>(Map.of(Place.A, TOKENS, Place.INNER1, TOKENS),
                Collections.emptySet(), Set.of(Place.B_MIRROR), Map.of(Place.INNER2, TOKENS)));
        transitions.add(new Transition<>(Map.of(Place.INNER2, TOKENS), Collections.emptySet(),
                Set.of(Place.B), Map.of(Place.INNER1, TOKENS)));
        transitions.add(new Transition<>(Map.of(Place.INNER1, TOKENS, Place.B_MIRROR, TOKENS), Collections.emptySet(),
                Collections.emptySet(), Map.of(Place.B, TOKENS, Place.INNER1, TOKENS)));
        transitions.add(new Transition<>(Map.of(Place.B, TOKENS, Place.INNER2, TOKENS), Collections.emptySet(),
                Collections.emptySet(), Map.of(Place.RESULT, TOKENS, Place.INNER2, TOKENS, Place.B_MIRROR, TOKENS)));

        return transitions;
    }

    private Collection<Transition<Place>> constructFinalTransition() {
        return Collections.singleton(new Transition<>(Map.of(Place.INNER1, TOKENS), Collections.emptySet(),
                Set.of(Place.A), Collections.emptyMap()));
    }

    private class MultiplyThread implements Runnable {
        private long fireCount = 0;

        private void printFireCount() {
            System.out.println("Thread " + Thread.currentThread().getName() + " fired " + fireCount + " times.");
        }

        @Override
        public void run() {
            try {
                while (!Thread.currentThread().isInterrupted()) {
                    petriNet.fire(transitions);
                    fireCount++;
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                printFireCount();
            }
        }
    }

    private Integer computeResult() {
        ExecutorService threadPool = Executors.newFixedThreadPool(THREADS);
        for (int i = 0; i < THREADS; i++) {
            threadPool.submit(new MultiplyThread());
        }

        try {
            petriNet.fire(finalTransition);
            threadPool.shutdownNow();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            System.err.println(e.getMessage());
        }

        return petriNet.getTokens(Place.RESULT);
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int val1 = sc.nextInt(), val2 = sc.nextInt();

        Main multiplicator = new Main(val1, val2);

        System.out.println("Result: " + multiplicator.computeResult());
    }
}
