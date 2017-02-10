#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HASHSIZE 101

struct nlist {	/* array record */
	struct nlist *next; /* next record */
	char *name; /* defined name */
	char *defn; /* replacing text */
};

static struct nlist *hashtab[HASHSIZE];  /* array of pointers */

struct nlist *lookup (char *);

unsigned hash (char *s) {
	unsigned hashval;
	for (hashval = 0 ; *s != '\0' ; s++)
		hashval = *s + 31 * hashval;
	return hashval % HASHSIZE;
}

struct nlist *lookup (char *s) {
	struct nlist *np;
	for (np = hashtab[hash(s)] ; np != NULL ; np = np->next)
		if (strcmp(s, np->name) == 0)
			return np;	/* found */
	return NULL;	/* not found */
}

/*
	Idiom for browsing linked lists:
	for (ptr = head ; ptr != NULL ; ptr = ptr->next)
		...	
*/

struct nlist *install (char *name, char *defn) {
	struct nlist *np;
	unsigned hashval;

	if ((np = lookup(name)) == NULL) { 	/* not found */
		np = (struct nlist *) malloc (sizeof(*np));
		if (np == NULL || (np->name = strdup(name)) == NULL)
			return NULL;
		hashval = hash (name);
		np->next = hashtab[hashval];
		hashtab[hashval] = np;
	} else /* found */
		free ((void*) np->defn); /* remove previous definition */
	if ((np->defn = strdup(defn)) == NULL)
		return NULL;
	return np;
}

void undef (char *name) {
	struct nlist *np;
	if ((np = lookup(name)) != NULL) {
		free (np->defn);
		free (np->name);
	}
}

int main () {
	struct nlist *np = install ("test", "tested123");
	printf ("%s %s\n", np->name, np->defn);
	printf ("%d -> %s\n", hash ("test"), hashtab[hash("test")]->name);
	printf ("%s\n", lookup ("test")->defn);
	undef ("test");
	if (lookup ("test") == NULL)
		puts ("not found");

	return 0;
}