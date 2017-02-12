#include <unistd.h>
#include <fcntl.h>

#define BUFSIZE 1000

int main (int argc, char *argv[]) {
	int fd, n, i;
	char buf[BUFSIZE];
	if (argc >= 1) {
		for (i = 1 ; i < argc ; ++i) {
			fd = open (argv[i], O_RDONLY, 0);
			while ((n = read (fd, buf, BUFSIZE)) > 0)
				write (0, buf, n);
		}
	}

	return 0;
}