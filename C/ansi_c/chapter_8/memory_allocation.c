
typedef long Align;		/* alignment based on long */

typedef union header { 	/* block header */
	struct {
		union header *ptr;	/* next block on the list */
		unsigned size;	/* block size */
	} s;
	Align x;	/* force block alignment */
} Header;

/* field Align x is not used, it just forces data alignment so that the memory block can store even the biggest types */

static Header base; /* empty list at the beginning */
static Header *freep = NULL;	/* beginning of the list of empty blocks */

void *malloc (unsigned nbytes) {
	Header *p, *prevp;
	Header *morecore (unsigned);
	unsigned nunits;

	nunits = (nbytes+sizeof(Header)-1)/sizeof(Header)+1;
	if ((prevp = freep) == NULL) { /* there is no list yet */
		base.s.ptr = freep = prevptr = &base;
		base.s.size = 0;
	}
	for (p = prevp->s.ptr ; ; prevp = p, p = p->s.ptr) {
		if (p->s.size >= nunits) { 		/* large enough */
			if (p->s.size == nunits)	/* exactly the same size */
				prevp->s.ptr = p->s.ptr;
			else {
				p->s.size -= nunits;
				p += p->s.size;
				p->s.size = nunits;
			}
			freep = prevp;
			return (void*)(p+1);
		}
		if (p == freep)	/* empty block list */
			if ((p = morecore(nunits)) == NULL)
				return NULL;	/* no blocks */
	}
}

#define NALLOC 1024	/* minimum amount to be taken from the system */

static Header *morecore (unsigned nu) {
	char *cp, *sbrk (int);
	Header *up;

	if (nu < NALLOC)
		nu = NALLOC;
	cp = sbrk (nu * sizeof (Header));
	if (cp == (char*)-1) /* no space at all */
		return NULL;
	up = (Header*) cp;
	up->s.size = nu;
	free ((void*)(up+1));
	return freep;
}

void free (void *ap) {
	Header *bp, *p;
	bp = (Header*) ap-1;	/* block header */
	for (p = freep ; !(bp > p && bp < p->s.ptr) ; p = p->s.ptr)
		if (p >= p->s.ptr && (bp > p || bp < p->s.ptr))
			break;	/* freeing a block at the beginning or end */
	if (bp + bp->size == p->s.ptr) { /* merging with the next */
		bp->s.size += p->s.ptr->s.size;
		bp->s.ptr = p->s.ptr->s.ptr;
	} else
		bp->s.ptr = p->s.ptr;
	if (p + p->size == bp) {
		p->s.size += bp->s.size;
		p->s.ptr = bp->s.ptr;
	} else
		p->s.ptr = bp;
	freep = p;
}

int main () {

}