#include <gtk/gtk.h>


void display(GtkWidget *widget, gpointer label)
{
	gtk_label_set_text(GTK_LABEL(label), "Hello world!");
}

int main(int argc, char** argv){
  GtkWidget *label;
  GtkWidget *window;
  GtkWidget *frame;
  GtkWidget *button;

  gtk_init(&argc, &argv);
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  
  frame = gtk_fixed_new();
  gtk_container_add(GTK_CONTAINER(window), frame);

  button = gtk_button_new_with_label("click");
  gtk_widget_set_size_request(button, 80, 35);
  gtk_fixed_put(GTK_FIXED(frame), button, 50, 80);

  label = gtk_label_new(" ");
  gtk_fixed_put(GTK_FIXED(frame), label, 190, 58);
  
  gtk_widget_show_all(window);
  
  g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);

  g_signal_connect(button, "clicked", G_CALLBACK(display), label);

  gtk_main();
  return 0;
}