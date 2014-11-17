#include <gtk/gtk.h>

void display(GtkWidget *widget, gpointer comboAndLabel)
{
  GtkWidget **widgets = comboAndLabel; 
  gchar *string;
  string = gtk_entry_get_text (GTK_ENTRY (GTK_COMBO (widgets[0])->entry));
  gtk_label_set_text(GTK_LABEL(widgets[1]), string);
}

int main(int argc, char** argv){
  GtkWidget *label;
  GtkWidget *window;
  GtkWidget *frame;
  GtkWidget *combo;
  GtkWidget *button;
  GtkWidget *comboAndLabel[2];
  
  gtk_init(&argc, &argv);
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  frame = gtk_fixed_new();
  gtk_container_add(GTK_CONTAINER(window), frame);
  /* combo box */
  combo = gtk_combo_new();
  gtk_container_add(GTK_CONTAINER(frame), combo);
  GList *glist = NULL;
  glist = g_list_append (glist, "selection1");
  glist = g_list_append (glist, "selection2");
  glist = g_list_append (glist, "selection3");
  gtk_combo_set_popdown_strings (GTK_COMBO (combo), glist);  
  gtk_fixed_put(GTK_FIXED(frame), combo, 50, 20);
 
  button = gtk_button_new_with_label("click");
  gtk_fixed_put(GTK_FIXED(frame), button, 50, 80);
  
  /* label that display combo box selection */
  label = gtk_label_new(" ");
  gtk_fixed_put(GTK_FIXED(frame), label, 190, 58);

  gtk_widget_show_all(window);
  
  g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
  comboAndLabel[0] = combo;
  comboAndLabel[1] = label;
  g_signal_connect(button, "clicked", G_CALLBACK(display), &comboAndLabel);
  
  gtk_main();
  
  return 0;
}