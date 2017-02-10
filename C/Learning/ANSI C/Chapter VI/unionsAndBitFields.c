#include <stdio.h>

enum {
	KEYWORD = 01,
	EXTERNAL = 02,
	STATIC = 04
};

int main () {
	/* By Using Masks */
	{
		int flags = 0;
		flags |= EXTERNAL | STATIC; /* turns EXTERNAL & STATIC on */
		flags &= ~(EXTERNAL | STATIC); /* turns them off */
		if ((flags & (EXTERNAL | STATIC)) == 0) /* checks if both are turned off */
			;
	}

	/* By Using Bit Field */
	struct {
		unsigned int is_keyword : 1;
		unsigned int is_extern : 1;
		unsigned int is_static : 1;
	} flags;

	flags.is_extern = flags.is_static = 1;	/* turns is_extern and is_static on */
	flags.is_extern = flags.is_static = 0;	/* turns them off */
	if (flags.is_extern == 0 && flags.is_static == 0) /* checks if both are turned off */
		;

	printf ("%d\n", sizeof(flags));
	return 0;
}