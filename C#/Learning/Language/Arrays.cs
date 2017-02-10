using System;

class Arrays {
  static void arrayOverview() {
    int[] array1 = new int[5];
    int[] array2 = new int[] {1,2,3,4,5};
    int[] array3 = {1,2,3,4,5};
    int[,] multiDimensionalArray1 = new int[5, 3];
    int[,] multiDimensionalArray2 = { {1,2,3}, {4,5,6} };
    int[][] jaggedArray = new int[6][];
    jaggedArray[0] = new int[4] {1,2,3,4};
    // jagged array is an array of arrays
  }

  static void arraysAsObjects() {
    int[] numbers = {1, 2, 3, 4, 5};
    Console.WriteLine(numbers.Length);
    int[,] nums2 = new int[5, 10];
    // Rank property displays the number of dimensions of an array
    Console.WriteLine(nums2.Rank);
  }

  static void multiDimArrays() {
    int[, ,] array2 = new int[4,2,3];
    int[,] array4 = { {1,2}, {2,3}, {3,4}, {5,6} };
    Console.WriteLine(array4[2, 0]);
  }

  static void jaggedArrays() {
    int[][] jaggedArray = new int[3][];
    jaggedArray[0] = new int[5];
    jaggedArray[1] = new int[4];
    jaggedArray[2] = new int[3];
    // jaggedArray[0] = new int[] {1,2,3,4,5};
    int[][] jaggedArray3 = { new int[] {1,2,3}, new int[] {4,5}, new int[] {6,7} };
    int a = jaggedArray3[0][1];
    int[][,] jaggedArray4 = new int[3][,] {
      new int[,] { {1,2}, {3,4} },
      new int[,] { {5,6}, {6,7} },
      new int[,] { {8,9}, {9,0} }
    };
    Console.WriteLine(jaggedArray4[0][1,1]);
    // For jagged arrays, length returns number of arrays contained in them
    Console.WriteLine(jaggedArray4.Length);
  }

  static void printArray(ref int[] arr) {
    foreach (int a in arr)  Console.Write(a + " ");
    Console.WriteLine();
  }

  static void print2DArray(int[,] arr) {
    foreach (int a in arr)  Console.Write(a + " ");
    Console.WriteLine();
  }

  static void passingArraysAsArguments() {
    int[] array = {1, 2, 3, 4};
    printArray(ref array);
    // printArray(new int[] {4, 3, 2, 1});
    print2DArray(new int[,] { {1,2}, {3,4}, {5,6}});
  }

  static void implicitlyTypedArrays() {
    var a = new[] { 1, 2, 3, 4, 5 }; // int[]
    var b = new[] { "a", "b", "c", "d" }; // string[]
  }

  static void Main() {
    arrayOverview();
    arraysAsObjects();
    multiDimArrays();
    jaggedArrays();
    passingArraysAsArguments();
  }
}
