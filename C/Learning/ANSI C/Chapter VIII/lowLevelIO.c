#include <unistd.h>

/* reads n bytes from position pos */
int get (int fd, long pos, char *buf, int n) {
	if (lseek (fd, pos, 0) >= 0) /* get to pos */
		return read (fd, buf, n);
	else
		return -1;
}

int main () {
	char buf[1000];

	/* long lseek (int fd, long offset, int origin);
	lseek (fd, 0L, 2); // end of the file
	lseek (fd, 0L, 0); // beginning of the file
	*/

	return 0;
}
