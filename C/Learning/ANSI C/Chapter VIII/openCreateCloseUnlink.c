#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

int main () {
	int fd, fd2;
	/* int open (char *name, int flags, int perms); */
	/*
		name - file name
		flags {
			O_RDONLY - open read only
			O_WRONLY - open write only
			O_RDWR - open read & write
		}
		perms = 0
	*/
	/* fd = open (name, flags, perms); */

	printf ("%d\n", fd);
	fd = open ("test.txt", O_RDONLY, 0);
	printf ("%d\n", fd);

	/* fd2 = creat (name, perms); */

	fd2 = creat ("hehe.txt", 0);
	printf ("%d\n", fd2);

	unlink ("hehe.txt");





	return 0;
}