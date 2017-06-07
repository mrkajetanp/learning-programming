import com.sun.org.apache.xpath.internal.SourceTree;

import java.util.*;

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
}
