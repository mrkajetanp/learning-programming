#include <gtk/gtk.h>

int main(int argc, char *argv[]) {

  GtkWidget *window;
  GdkColor color;

  color.red = 0x2828;
  color.green = 0x2828;
  color.blue = 0x2828;

  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(window), "Hello Title!");
  gtk_widget_modify_bg(window, GTK_STATE_NORMAL, &color);

  gtk_widget_show_all(window);

  g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);

  gtk_main();

  return 0;
}
