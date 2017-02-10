#define NAME_MAX 14		/* max name length allowed */

typedef struct {	/* directory entry, system independent */
	long ino;	/* i-node number */
	char name[NAME_MAX+1];	/* name + '\0' */
} Dirent;

typedef struct {
	int fd;		/* directory file descriptor */
	Dirent d;	/* file entry */
} DIR;

DIR *opendir (char *dirname);
Dirent *readdir (DIR *dfd);
void closedir (DIR *dfd);

#include <stdio.h>
#include <string.h>
#include <fcntl.h>	/* read and write */
#include <sys/types.h>	/* typedefs */
#include <sys/stat.h>	/* structure returned by stat */

void fsize (char *);

int main (int argc, char **argv) {
	if (argc == 1)	/* default: current directory */
		fsize (".");
	else
		while (--argc > 0)
			fsize (*++argv);
	return 0;
}

int stat (char *, struct stat *);
void dirwalk (char *, void (*fcn)(char *));

void fsize (char *name) {	/* prints file size */
	struct stat stbuf;
	if (stat(name, &stbuf) == -1) {
		fprintf (stderr, "fsize: can't access %s\n", name);
		return;
	}
	if ((stbuf.st_mode & S_IFMT) == S_IFDR)
		dirwalk (name, fsize);
	printf ("%8ld %s\n", stbuf.st_size, name);
}

#define MAX_PATH 1024

void dirwalk (char *dir, void (*fcn)(char *)) {
	char name[MAX_PATH];
	Dirent *dp;
	DIR *dfd;

	if ((dfd = opendir(dir)) == NULL) {
		fprintf (stderr, "dirwalk: can't open %s\n", dir);
		return;
	} 

	while ((dp = readdir (dfd)) != NULL) {
		if (strcmp (dp->name, ".") == 0 || strcmp (dp->name, "..") == 0)
			continue; /* skip self and parent */
		if (strlen(dir)+strlen(dp->name)+2 > sizeof(name))
			fprintf (stderr, "dirwalk: name %s %s is too long\n", dir, dp->name);
		else {
			sprintf (name, "%s/%s", dir, dp->name);
			(*fcn)(name);
		}
	}
	closedir (dfd);
}

int fstat (int fd, struct stat *);

DIR *opendir (char *dirname) {
	int fd;
	struct stat stbuf;
	DIR *dp;

	if ((fd = open(dirname, O_RDONLY, 0)) == -1
		|| fstat (fd, &stbuf) == -1
		|| (stbuf.st_mode & S_IFMT) != S_IFDIR
		|| (dp = (DIR *) malloc (sizeof(DIR))) == NULL)
			return NULL;

		dp->fd = fd;
		return dp;
}

void closedir (DIR *dp) {
	if (dp) {
		close (dp->fd);
		free (dp);
	}
}

#include <sys/dir.h>

Dirent *readdir (DIR *dp) {
	struct direct dirbuf;	/* local directory structure */
	static Dirent d;	/* platform independent structure */

	while (read(dp->fd, (char*) &dirbuf, sizeof(dirbuf)) == sizeof(dirbuf)) {
		if (dirbuf.d_ino == 0)	/* slot in use */
			continue;
		d.ino = dirbuf.d_ino;
		strncpy (d.name, dirbuf.d_name, DIRSIZ);
		d.name[DIRSIZ] = '\0';
		return &d;
	}
	return NULL;
}