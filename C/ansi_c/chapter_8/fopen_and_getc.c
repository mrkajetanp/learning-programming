#include <fcntl.h>

#define NULL 0
#define EOF (-1)
#define BUFSIZ 1024
#define OPEN_MAX 20	/* max number of opened files */

typedef struct _iobuf {
	int cnt;	/* remaining characters */
	char *ptr;	/* next position */
	char *base;	/* buffer */
	int flag;	/* access mode flag */
	int fd;		/* file descriptor */
} FILE;

extern FILE _iob[OPEN_MAX];

#define stdin (&_iob[0])
#define stdout (&_iob[1])
#define stderr (&_iob[2])

enum _flags {
	_READ = 01, 	/* file opened for reading */
	_WRITE = 02,	/* file opened for writing */
	_UNBUF = 04,	/* unbuffered file */
	_EOF = 010,		/* end of file */
	_ERR = 020		/* error */
};

int _fillbuf (FILE *);
int _flushbuf (int, FILE *);

#define feof(p) ((p)->flag & _EOF) != 0)
#define ferror(p) ((p)->flag & _ERR) != 0)
#define fileno(p) ((p)->fd)

#define getc(p) (--(p)->cnt >= 0) ? (unsigned char) *(p)->ptr++ : _fillbuf(p))
#define putc(x,p) (--(p)->cnt >= 0 ? *(p)->ptr++ = (x) : _flushbuf((x), p))

#define getchar() getc(stdin)
#define putchar(x) putc((x), stdout)

#define PERMS 0666 /* RW for the owner, group and others */

FILE *fopen (char *name, char *mode) {
	int fd;
	FILE *fp;

	if (*mode != 'r' && *mode != 'w' && *mode != 'a')
		return NULL;
	for (fp = _iob ; fp < _iob + OPEN_MAX ; fp++)
		if ((fp->flag & (_READ | _WRITE)) == 0)
			break;	/* found empty pointer */
	if (fp >= _iob + OPEN_MAX)	/* no empty pointers */
		return NULL;

	if (*mode == 'w')
		fd = creat (name, PERMS);
	else if (*mode == 'a') {
		if ((fd = open(name, O_WRONLY, 0)) == -1)
			fd = creat (name, PERMS);
		lseek (fd, 0L, 2);
	} else
		fd = open (name, O_RDONLY, 0);
	if (fd == -1)	/* no access to file */
		return NULL;
	fp->fd = fd;
	fp->cnt = 0;
	fp->base = NULL;
	fp->flag = (*mode == 'r') ? _READ : _WRITE;
	return fp;
}

int _fillbuf (FILE *fp) {
	int bufsize;

	if ((fp->flag & (_READ | _EOF_ERR)) != _READ)
		return EOF;
	bufsize = (fp->flag & _UNBUF) ? 1 : BUFSIZ;
	if (fp->base == NULL)	/* does not have a buffer yet */
		if ((fp->base = (char *) malloc (bufsize)) == NULL)
			return EOF;		/* can't create a buffer */
	fp->ptr = fp->base;
	fp->cnt = read (fp->fd, fp->ptr, bufsize);
	if (--fp->cnt < 0) {
		if (fp->cnt == -1)
			fp->flag |= _EOF;
		else
			fp->flag |= _ERR;
		fp->cnt = 0;
		return EOF;
	}
	return (unsigned char) *fp->ptr++;
}

FILE _iob[OPEN_MAX] = {
	{ 0, (char*) 0, (char*) 0, _READ, 0 },
	{ 0, (char*) 0, (char*) 0, _WRITE, 1 },
	{ 0, (char*) 0, (char*) 0, _WRITE, | _UNBUF, 2 }
};







