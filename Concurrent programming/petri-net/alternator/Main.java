package alternator;

import petrinet.PetriNet;
import petrinet.Transition;

import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Main {
    private static final Integer THREADS = 3;
    private static final Integer TOKENS = 1;
    private PetriNet<Place> petriNet;
    private Collection<Transition<Place>> entryA, exitA, entryB, exitB, entryC, exitC;

    private enum ThreadName {
        A, B, C;

        private static ThreadName[] values = values();

        private static ThreadName get(Integer i) {
            return values[i % values.length];
        }
    }

    private enum Place {
        A1, A2, A_FORBID, B1, B2, B_FORBID, C1, C2, C_FORBID, EXCL
    }

    private Main() {
        petriNet = new PetriNet<>(constructPlaces(), true);
        entryA = constructEntryTransition(Place.A2, Place.A1, Place.A_FORBID);
        entryB = constructEntryTransition(Place.B2, Place.B1, Place.B_FORBID);
        entryC = constructEntryTransition(Place.C2, Place.C1, Place.C_FORBID);
        exitA = constructExitTransition(Place.A1, Place.A2, Place.B_FORBID, Place.C_FORBID, Place.A_FORBID);
        exitB = constructExitTransition(Place.B1, Place.B2, Place.A_FORBID, Place.C_FORBID, Place.B_FORBID);
        exitC = constructExitTransition(Place.C1, Place.C2, Place.A_FORBID, Place.B_FORBID, Place.C_FORBID);
    }

    private Map<Place, Integer> constructPlaces() {
        return Map.of(Place.A2, TOKENS, Place.B2, TOKENS, Place.C2, TOKENS, Place.EXCL, TOKENS);
    }

    private Collection<Transition<Place>> constructEntryTransition(Place placeIn, Place placeOut, Place forbid) {
        return Collections.singleton(new Transition<>(Map.of(Place.EXCL, TOKENS, placeIn, TOKENS),
                Collections.emptySet(), Set.of(forbid), Map.of(placeOut, TOKENS)));
    }

    private Collection<Transition<Place>> constructExitTransition(Place placeIn, Place placeOut,
          Place reset1, Place reset2, Place forbid) {
        return Collections.singleton(new Transition<>(Map.of(placeIn, TOKENS), Set.of(reset1, reset2),
                Collections.emptySet(), Map.of(placeOut, TOKENS, Place.EXCL, TOKENS, forbid, TOKENS)));
    }

    // Asserts that the critical section is not shared in any of the markings.
    private void assertSafety(Set<Map<Place, Integer>> reachable) {
        reachable.forEach((map) -> {
            assert((map.getOrDefault(Place.A1, 0) | map.getOrDefault(Place.B1, 0)
                    | map.getOrDefault(Place.C1, 0)) <= 1);
        });
    }

    private Collection<Transition<Place>> getConcatenatedTransitions() {
        return Stream.of(entryA, entryB, entryC, exitA, exitB, exitC).
                flatMap(Collection::stream).
                collect(Collectors.toSet());
    }

    private void printReachableCount() {
        Set<Map<Place, Integer>> reachable = petriNet.reachable(getConcatenatedTransitions());
        assertSafety(reachable);
        System.out.println(reachable.size() + " markings are reachable.");
    }

    private class AlternateRunnable implements Runnable {
        private String threadNameString;
        private Collection<Transition<Place>> entryTransition;
        private Collection<Transition<Place>> exitTransition;

        private AlternateRunnable(ThreadName threadName) {
            threadNameString = String.valueOf(threadName);
            switch (threadName) {
                case A:
                    entryTransition = entryA;
                    exitTransition = exitA;
                    break;
                case B:
                    entryTransition = entryB;
                    exitTransition = exitB;
                    break;
                case C:
                    entryTransition = entryC;
                    exitTransition = exitC;
                    break;
            }
        }

        @Override
        public void run() {
            try {
                while(!Thread.currentThread().isInterrupted()) {
                    petriNet.fire(entryTransition);
                    System.out.print(threadNameString);
                    System.out.print(".");
                    petriNet.fire(exitTransition);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    private void alternate() {
        ExecutorService pool = Executors.newFixedThreadPool(THREADS);
        for (int i = 0; i < THREADS; i++) {
            pool.execute(new AlternateRunnable(ThreadName.get(i)));
        }

        pool.shutdown();
        try {
            if (!pool.awaitTermination(30, TimeUnit.SECONDS)) {
                pool.shutdownNow();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            System.err.println(e.getMessage());
        }
    }

    public static void main(String[] args) {
        Main alternator = new Main();

        alternator.printReachableCount();
        alternator.alternate();
    }
}
