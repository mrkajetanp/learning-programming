#include <stdio.h>
#include <glib.h>

void printStringKeyIntValue( gpointer key, gpointer value, gpointer userData ) {
  char* realKey = (char*)key;
  int realValue = GPOINTER_TO_INT(value);

  printf( "%s => %d\n", realKey, realValue );
  return;
}

void printIntKeyStrValue(gpointer key, gpointer value, gpointer userData) {
  int realKey = GPOINTER_TO_INT(key);
  char *realValue = (char*)value;

  printf("%d => %s\n", realKey, realValue);
  return;
}


int main(int argc, char **argv) {
  GHashTable *g_hash_table;

  g_hash_table = g_hash_table_new(g_int_hash, g_int_equal);

  g_hash_table_insert(g_hash_table, "Oxford", (int*)850);
  g_hash_table_insert(g_hash_table, "Cambridge", (int*)1209);

  printf("%d\n", g_int_hash("Cambridge"));
  printf("%d\n", g_int_hash("Oxford"));

  g_hash_table_foreach(g_hash_table, printStringKeyIntValue, NULL);

  return 0;
}
