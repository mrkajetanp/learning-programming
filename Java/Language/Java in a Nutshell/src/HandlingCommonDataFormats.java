import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.Month;
import java.time.Period;
import java.time.YearMonth;
import java.time.temporal.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Created by cajetan on 02/07/2017.
 */
public class HandlingCommonDataFormats {
    public static void run() {
        System.out.println("*** Handling Common Data Formats ***");

        strings();
        numbersAndMaths();
        dateAndTime();

        System.out.println();
    }


    static void strings() {
        // string literals

        System.out.println("Dog's length is: " + "dog".length());

        // string concatenation

        String a = "one";
        String b = "two";
        String c = a + " " + b;

        System.out.println(c);

        // regex

        Pattern p = Pattern.compile("honou?r");
        Matcher mUK = p.matcher("For Brutus is an honourable man.");
        Matcher mUS = p.matcher("For Brutus is an honorable man.");

        System.out.println("Matches UK spelling? " + mUK.find());
        System.out.println("Matches US spelling? " + mUS.find());

        // matches both "minimize" and "minimise"
        // Pattern.compile("minimi[sz]e");

        p = Pattern.compile("\\d");
        Matcher m = p.matcher("Apollo 13");
        System.out.println("matches?: " + m.find());
        System.out.println("group?: " + m.group());

        // using regexps with collections
        String[] inputs = { "Cat", "Dog", "Ice-9", "99 Luftballoons" };
        List<String> ls = Arrays.asList(inputs);
        List<String> withDigits = ls.stream().filter(p.asPredicate()).collect(Collectors.toList());
        System.out.println(withDigits);
    }

    static void numbersAndMaths() {
        // number's byte representation - two's complement
        byte b = (byte) 0b1111_1111; // -1
        System.out.println("b = " + b);
        b++;
        System.out.println("b = " + b);

        b = (byte) 0b1111_1110; // -2
        System.out.println("b = " + b);
        b++;
        System.out.println("b = " + b);

        b = (byte) 0b1000_0000; // -128
        System.out.println("b = " + b);

        // floating-point numbers

        double d = 0.3;
        double d2 = 0.2;
        System.out.println("d = " + d + ", d2 = " + d2);
        System.out.println("(d2 - d) = " + (d2-d));

        BigDecimal bd = new BigDecimal(d);
        System.out.println("bd = " + bd);

        bd = new BigDecimal(0.3);
        System.out.println("bd = " + bd);

        bd = new BigDecimal(BigInteger.ONE);
        try {
            bd.divide(new BigDecimal(3.0));
        } catch (ArithmeticException e) {
            System.err.println("Well it wasn't the brightest idea today..");
        }
        System.out.println("bd = " + bd);
    }

    enum Quarter {
        FIRST, SECOND, THIRD, FOURTH
    }

    static void dateAndTime() {
        class BirthdayDiary {
            private Map<String, LocalDate> birthdays;

            public BirthdayDiary() {
                this.birthdays = new HashMap<>();
            }

            public LocalDate addBirthday(String name, int day, int month, int year) {
                LocalDate birthday = LocalDate.of(year, month, day);
                birthdays.put(name, birthday);
                return birthday;
            }

            public LocalDate getBirthdayFor(String name) {
                return birthdays.get(name);
            }

            public int getAgeInYear(String name, int year) {
                Period period = Period.between(birthdays.get(name), birthdays.get(name).withYear(year));
                return period.getYears();
            }

            public Set<String> getFriendsOfAgeIn(int age, int year) {
                return birthdays.keySet().stream()
                        .filter(p -> getAgeInYear(p, year) == age)
                        .collect(Collectors.toSet());
            }

            public int getDaysUntilBirthday(String name) {
                Period period = Period.between(LocalDate.now(), birthdays.get(name));
                return period.getDays();
            }

            public Set<String> getBirthdaysIn(Month month) {
                return birthdays.entrySet().stream()
                        .filter(p -> p.getValue().getMonth() == month)
                        .map(p -> p.getKey())
                        .collect(Collectors.toSet());
            }

            public int totalAgeInYears() {
                return birthdays.keySet().stream()
                        .mapToInt(p -> getAgeInYear(p, LocalDate.now().getYear()))
                        .sum();
            }

        }

        LocalDate today = LocalDate.now();
        Month currentMonth = today.getMonth();
        Month firstMonthOfQuarter = currentMonth.firstMonthOfQuarter();

        class QuarterOfYearQuery implements TemporalQuery<Quarter> {
            @Override
            public Quarter queryFrom(TemporalAccessor temporal) {
                LocalDate now = LocalDate.from(temporal);

                if (now.isBefore(now.with(Month.APRIL).withDayOfMonth(1)))
                    return Quarter.FIRST;
                else if (now.isBefore(now.with(Month.JULY).withDayOfMonth(1)))
                    return Quarter.SECOND;
                else if (now.isBefore(now.with(Month.NOVEMBER).withDayOfMonth(1)))
                    return Quarter.THIRD;
                else
                    return Quarter.FOURTH;
            }
        }

        QuarterOfYearQuery q = new QuarterOfYearQuery();

        // Direct
        Quarter quarter = q.queryFrom(LocalDate.now());
        System.out.println(quarter);

        // Indirect

        quarter = LocalDate.now().query(q);
        System.out.println(quarter);

        /* Adjusters */

        class FirstDayOfTheQuarter implements TemporalAdjuster {
            @Override
            public Temporal adjustInto(Temporal temporal) {
                final int currentQuarter = YearMonth.from(temporal).get(IsoFields.QUARTER_OF_YEAR);

                switch (currentQuarter) {
                    case 1:
                        return LocalDate.from(temporal)
                                .with(TemporalAdjusters.firstDayOfYear());

                    case 2:
                        return LocalDate.from(temporal)
                                .withMonth(Month.APRIL.getValue())
                                .with(TemporalAdjusters.firstDayOfMonth());

                    case 3:
                        return LocalDate.from(temporal)
                                .withMonth(Month.JULY.getValue())
                                .with(TemporalAdjusters.firstDayOfMonth());

                    case 4:
                        return LocalDate.from(temporal)
                                .withMonth(Month.OCTOBER.getValue())
                                .with(TemporalAdjusters.firstDayOfMonth());

                    default:
                        return null;
                }
            }
        }

        LocalDate now = LocalDate.now();
        Temporal fdoq = now.with(new FirstDayOfTheQuarter());
        System.out.println(fdoq);
    }

}














