package JavaGradleTest;

public class App {
    public String getGreeting() {
        return "Hello world. Please test me now.";
    }

    public static void main(String[] args) {
        System.out.println(new App().getGreeting());
    }
}
