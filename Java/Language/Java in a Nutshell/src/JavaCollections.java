import com.sun.org.apache.xpath.internal.SourceTree;

import java.awt.*;
import java.lang.reflect.Array;
import java.util.*;
import java.util.List;

/**
 * Created by cajetan on 6/7/17.
 */
public class JavaCollections {
    public static void run() {
        System.out.println("*** Collections ***");

        maps();

        System.out.println();
    }

    public static void maps() {
        Map<String, Integer> mapOne = new HashMap<>();
        Map<String, Integer> singleton = Collections.singletonMap("test", -1);
        Map<String, Integer> empty = Collections.emptyMap();

        String[] words = { "this", "is", "a", "test" };

        for (int i = 0; i < words.length ; i++)
            mapOne.put(words[i], i);

        mapOne.putAll(singleton);

        /*
        for (int i = 0 ; i < words.length ; i++) {
            if (mapOne.get(words[i]) != i)
                throw new AssertionError();
        }
        */

        assert mapOne.containsKey(words[0]);
        assert !mapOne.containsValue(words.length);

        Set<String> keys = mapOne.keySet();
        Collection<Integer> values = mapOne.values();
        Set<Map.Entry<String, Integer>> entries = mapOne.entrySet();

        System.out.printf("Map:%s%nKeys: %s%nValues: %s%nEntries: %s%n", mapOne, keys, values, entries);

        for (String key : mapOne.keySet())
            System.out.print(key + " ");
        System.out.println();

        for (Integer value : mapOne.values())
            System.out.print(value + " ");
        System.out.println();

        for (Map.Entry<String, Integer> pair : mapOne.entrySet()) {
            System.out.printf("'%s' ==> %d%n", pair.getKey(), pair.getValue());
            pair.setValue(pair.getValue() + 1);
        }

        mapOne.put("testing", null);
        mapOne.get("testing");
        assert mapOne.containsKey("testing");
        mapOne.remove("testing");
        assert mapOne.get("testing") == null;
        assert !mapOne.containsKey("testing");

        mapOne.keySet().remove(words[0]);
        mapOne.values().remove(2);
        mapOne.values().removeAll(Collections.singleton(4));
        mapOne.values().retainAll(Arrays.asList(2, 3));

        Iterator<Map.Entry<String, Integer>> iter = mapOne.entrySet().iterator();

        while (iter.hasNext()) {
            Map.Entry<String, Integer> e = iter.next();

            if (e.getValue() == 2)
                iter.remove();
        }

        Set<Integer> v = new HashSet<>(mapOne.values());
        v.retainAll(singleton.values());

        mapOne.clear();
        assert mapOne.size() == 0;
        assert mapOne.isEmpty();
        assert mapOne.equals(empty);
    }

    public static void wrappers() {
        List<String> sync_list = Collections.synchronizedList(new ArrayList<String>());
        Set<Integer> set = Collections.synchronizedSet(new HashSet<Integer>());
        Map<String,Integer> map = Collections.synchronizedMap(new HashMap<String,Integer>());

        List<Integer> primes = new ArrayList<Integer>();
        List<Integer> readonly = Collections.unmodifiableList(primes);
        primes.addAll(Arrays.asList(2, 3, 5, 7, 11, 13, 17, 19));
        // readonly.add(23); // unsupported operation exception


    }

    public static void utilities() {
        List<Integer> list1 = Arrays.asList(1, 8, 4, 3, 9, 0);
        List<Integer> list2 = Arrays.asList(1, 9);

        Collections.sort(list1);
        int pos = Collections.binarySearch(list1, 3);

        Collections.copy(list1, list2);
        Collections.fill(list2, 8);
        Collections.max(list1);
        Collections.min(list1);

        Collections.reverse(list1);
        Collections.shuffle(list2);

        Set<Integer> s1 = Collections.emptySet();
        List<String> s2 = Collections.emptyList();
        Map<String, Integer> m = Collections.emptyMap();

        List<Integer> tenzeros = Collections.nCopies(10, 0);
    }

    public static void arraysAndHelpers() {
        String[] a = { "this", "is", "a", "test" };
        List<String> l = Arrays.asList(a);
        List<String> m = new ArrayList<>(l);

        String[] c = l.toArray(new String[0]);

        char[] text = "Now is the time".toCharArray();
        char[] copy = new char[100];
        System.arraycopy(text, 4, copy, 0, 10);
        System.arraycopy(copy, 3, copy, 6, 7);

        int[] intArray = new int[] { 10, 5, 7, -3 };
        Arrays.sort(intArray);
        int pos = Arrays.binarySearch(intArray, 7);

        int[] clone = intArray.clone();
        boolean b1 = Arrays.equals(intArray, clone);

        byte[] data = new byte[100];
        Arrays.fill(data, (byte) -1);
        Arrays.fill(data, 5, 10, (byte) -2);

        Class type = data.getClass();
        if (type.isArray()) {
            Class elementType = type.getComponentType();
        }
    }
}
