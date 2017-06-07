/**
 * Created by cajetan on 5/30/17.
 */

class GracefulCompletion {
    private volatile boolean shutdown = false;

    public void shutdown() {
        shutdown = true;
    }

    public void run() {
        while (!shutdown) {
            // ... process another task
        }
    }
}

public class MemoryAndConcurrency {
    public static void run() {
        System.out.println("*** Memory and Concurrency ***");

        concurrencyBasics();

        System.out.println();
    }

    static void concurrencyBasics() {

        Thread t = new Thread(() -> {
            System.out.println("Hello Thread!");
        });

        t.start();

        try {
            Thread.sleep(200);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        Thread handleThread =
                new Thread(() -> { throw new UnsupportedOperationException(); });

        handleThread.setName("My Broken Thread");

        handleThread.setUncaughtExceptionHandler((x, e) -> {
            System.err.printf("Exception in thread %d '%s':" +
            "%s at line %d of %s%n",
                    x.getId(),
                    x.getName(),
                    e.toString(),
                    e.getStackTrace()[0].getLineNumber(),
                    e.getStackTrace()[0].getFileName());
        });

        // handleThread.start();
    }
}
