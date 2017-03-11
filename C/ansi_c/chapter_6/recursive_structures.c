#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define MAXWORD 100

struct tnode {	/* tree node */
	char *word; /* pointer to the word */
	int count;	/* occurrence counter */
	struct tnode *left;		/* left child */
	struct tnode *right;	/* right child */
};

struct t {
	/* ... */
	struct s *p;	/* pointer to s */
};

struct s {
	/* ... */
	struct t *q;	/* pointer to t */
};

struct tnode *addTree (struct tnode *, char *);
void printTree (struct tnode *);
int getword (char *, int);
struct tnode *talloc (void);
char *strdup (char *);

struct tnode *addTree (struct tnode *p, char *w) {
	int cond;
	if (p == NULL) { /* new word */
		p = talloc ();
		p->word = strdup (w);
		p->count = 1;
		p->left = p->right = NULL;
	} else if ((cond = strcmp(w, p->word)) == 0)
		p->count++;
	else if (cond < 0)
		p->left = addTree (p->left, w);
	else
		p->right = addTree (p->right, w);
	return p;
}

void printTree (struct tnode *p) {
	if (p != NULL) {
		printTree (p->left);
		printf ("%4d %s\n", p->count, p->word);
		printTree (p->right);
	}
}

struct tnode *talloc (void) {
	return (struct tnode *) malloc (sizeof(struct tnode));
}

char *strdup (char *s) {
	char *p = (char *) malloc (strlen(s)+1); /* +1 for '\0' */
	if (p != NULL)
		strcpy (p, s);
	return p;
}

int main () {
	struct tnode *root;
	char word[MAXWORD];

	root = NULL;
	while (getword(word, MAXWORD) != EOF)
		if (isalpha(word[0]))
			root = addTree (root, word);
	printTree (root);

	return 0;
}