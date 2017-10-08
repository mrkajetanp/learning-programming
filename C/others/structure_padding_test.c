#include <stdio.h>
#include <string.h>

typedef struct _testS {
    char text[8];
    int arr[2];
} TestS;

void save_data(char* name) {
    FILE* f = fopen(name, "wb");

    char text[] = "aoeuaex";
    int arr[] = {1, 2};

    fwrite(text, sizeof text[0], sizeof(text), f);
    fwrite(arr, sizeof arr[0], 2, f);

    strcpy(text, "htnshex");
    arr[0] = 3;
    arr[1] = 4;

    fwrite(text, sizeof text[0], sizeof(text), f);
    fwrite(arr, sizeof arr[0], 2, f);

    fclose(f);
}

void read_data(char* name) {
    FILE* f = fopen(name, "rb");

    TestS ts1;
    printf("size: %ld\n", sizeof(ts1));

    fread(&ts1, sizeof(ts1), 1, f);
    printf("{%s - %d, %d}\n", ts1.text, ts1.arr[0], ts1.arr[1]);

    fread(&ts1, sizeof(ts1), 1, f);
    printf("{%s - %d, %d}\n", ts1.text, ts1.arr[0], ts1.arr[1]);

    fclose(f);
}

int main() {

    /* save_data("test_f.bin"); */
    read_data("test_f.bin");

    return 0;
}
