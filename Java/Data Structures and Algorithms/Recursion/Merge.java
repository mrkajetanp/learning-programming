public class Merge {
    public static void merge (int[] arrA, int sizeA,
                              int[] arrB, int sizeB, int[] arrC) {

        int aDex = 0, bDex = 0, cDex = 0;

        while (aDex < sizeA && bDex < sizeB) { // neither array is empty
            if (arrA[aDex] < arrB[bDex])
                arrC[cDex++] = arrA[aDex++];
            else
                arrC[cDex++] = arrB[bDex++];
        }

        while (aDex < sizeA) // arrB is empty but arrA is not
            arrC[cDex++] = arrA[aDex++];

        while (bDex < sizeB) // arrA is empty but arrB is not
            arrC[cDex++] = arrB[bDex++];
    }

    public static void display (int[] arr, int size) {
        for (int i = 0; i < size ; ++i)
            System.out.print (arr[i] + " ");
        System.out.println ("");
    }

    public static void main (String[] args) {
        int[] arrA = { 23, 59, 47, 91 };
        int[] arrB = { 7, 14, 39, 55, 62, 33 };
        int[] arrC = new int[10];

        merge (arrA, 4, arrB, 6, arrC);
        display (arrC, 10);
    }
}
