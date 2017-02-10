#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

int htoi (char s[]) {
    int i;
    if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X'))
        s += 2;
    int hex[20], len = strlen(s), result = 0;
    for (i = 0 ; i < len ; ++i) {
        if (isdigit(s[i]))
            hex[i] = s[i]-48;
        else
            hex[i] = tolower(s[i])-87;
    }
    for (i = 0 ; i < len ; ++i)
        result += hex[i]*pow(16, (len-1)-i);
    return result;
}

int main () {
    printf("%d\n", htoi("7de"));
    printf("%d\n", htoi("7DE"));
    printf("%d\n", htoi("0x7de"));
    printf("%d\n", htoi("1d9"));
    printf("%d\n", htoi("1D9"));
    printf("%d\n", htoi("10cE"));
    printf("%d\n", htoi("10ce"));

    return 0;
}
