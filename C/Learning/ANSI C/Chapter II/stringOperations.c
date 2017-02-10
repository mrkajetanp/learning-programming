#include <stdio.h>
#include <string.h>
#include <ctype.h>

void squeeze(char s1[], char s2[]) {
    int i, j, k;
    for (i = 0 ; s2[i] != '\0' ; ++i) {
        for (k = j = 0 ; s1[j] != '\0' ; ++j) {
            if (s1[j] != s2[i])
                s1[k++] = s1[j];
        }
        s1[k] = '\0';
    }
}

int any (char s1[], char s2[]) {
    int i, j;
    for (i = 0 ; s2[i] != '\0' ; ++i)
        for (j = 0 ; s1[j] != '\0' ; ++j)
            if (s1[j] == s2[i])
                return(j);
    return(-1);
}

int bitcount (unsigned x) {
	int b;
	for (b = 0 ; x != 0 ; x >>= 1)
			if (x & 01)
					b++;
	return b;
}

char lower (char c) {
	return (c >= 65 && c <= 90) ? c += 32 : c;
}

int binarySearch (int x, int v[], int n) {
	int low, high, mid;
	low = 0;
	high = n - 1;
	while (low <= high) {
		mid = (low+high)/2;
		if (x < v[mid])
			high = mid + 1;
		else if (x > v[mid])
			low = mid + 1;
		else
			return mid;
	}
	return -1;
}

void enEscape (char s[], char t[]) {
	int i, j;
	for (i = j = 0 ; s[i] != '\0' ; ++i, ++j) {
		switch (s[i]) {
			case '\n':
				t[j] = '\\';
				t[j+1] = 'n';
				++j;
				break;
			case '\t':
				t[j] = '\\';
				t[j+1] = 't';
				++j;
				break;
			default:
				t[j] = s[i];
				break;
		}
	}
	t[j] = '\0';
}

void deEscape (char s[], char t[]) {
	int i, j;
	for (i = j = 0 ; s[i] != '\0' ; ++i, ++j) {
		if (s[i] == '\\')
			switch (s[i+1]) {
				case 'n':
					t[j] = '\n';
					++i;
					break;
				case 't':
					t[j] = '\t';
					++i;
					break;
				default:
					t[j] = s[i];
			}
		else
			t[j] = s[i];
	}
	t[j] = '\0';
}

void expand (char s1[], char s2[]) {
	int i, j, k = 0, last = ' ';
	for (i = 0 ; s1[i] != '\0' ; ++i) {
		if ((isalpha(s1[i]) && s1[i+1] == '-' && isalpha(s1[i+2]) && s1[i+3] == '-' && isalpha(s1[i+4])) ||
				((isalpha(s1[i]) && s1[i+1] == '-' && isalpha(s1[i+2]) && s1[i+3] == '-' && isalpha(s1[i+4])))) {
			for (j = 0 ; last < s1[i+4] ; ++j, ++k)
				s2[k] = last = s1[i]+j;
			last = '|';
		}
		else if ((isalpha(s1[i]) && s1[i+1] == '-' && isalpha(s1[i+2])) ||
				(isdigit(s1[i]) && s1[i+1] == '-' && isdigit(s1[i+2]))) {
			for (j = 0 ; last < s1[i+2] ; ++j, ++k)
				s2[k] = last = s1[i]+j;
			last = ' ';
		}
	}
	s2[k] = '\0';
}

void reverse (char s[]) {
    int c, i, j;
    for (i = 0, j = strlen(s)-1 ; i < j ; ++i, --j) {
        c = s[i];
        s[i] = s[j];
        s[j] = c;
    }
}

void itob (int n, char s[], int b) {
    int i;
    switch (b) {
        case 2:
            for (i = 0 ; n > 0 ; ++i) {
                s[i] = n%2+'0';
                n /= 2;
            }
            break;
        case 8:
            for (i = 0 ; n > 0 ; ++i) {
                s[i] = n%8+'0';
                n /= 8;
            }
            break;
        case 16:
            for (i = 0 ; n > 0 ; ++i) {
                if (n%16 <= 9)
                    s[i] = n%16+'0';
                else if (n%16 >= 10 && n%16 <= 15)
                    s[i] = '7'+n%16;
                n /= 16;
            }
            break;
    }
    s[i] = '\0';
    reverse(s);
}

int main () {
    char str[] = "1122334455aoeuoaeucoeu";
    char str2[] = "1122334455aoeuoaeucoeu";
    char str3[] = "ABCDZa";
    int arr[] = {1,2,3,4,5};
    squeeze(str, "4aoe2");
    printf("%s\n", str);
    printf("%d\n", any(str2, ""));
    printf("%d\n", bitcount(8));
    printf("%d\n", bitcount(19));
    printf("%c%c%c%c\n", lower(str3[0]), lower(str3[1]), lower(str3[4]), lower(str3[5]));
    printf("Pos: %d\n", binarySearch(3,arr,5));
    char esc1[] = "something\ncomes\there\t";
    char esc2[40];
    char esc3[40];
    printf("esc1: %s\n", esc1);
    enEscape(esc1, esc2);
    printf("esc2: %s\n", esc2);
    deEscape(esc2, esc3);
    printf("esc3: %s\n", esc3);
    char exp1[] = "-a-z0-9";
    char exp2[40];
    expand(exp1, exp2);
    printf("%s => %s\n", exp1, exp2);
    char resbin[20], reshex[20], resoct[20];
    itob(156, resbin, 2);
    itob(156, resoct, 8);
    itob(156, reshex, 16);
    printf("%d => %s => %s => %s\n", 156, resbin, resoct, reshex);
    return 0;
}
