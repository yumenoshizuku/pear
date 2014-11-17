#include <gtk/gtk.h>

void show_info(GtkWidget *widget, gpointer window)
{
  GtkWidget *dialog;
  dialog = gtk_message_dialog_new(window,
            GTK_DIALOG_DESTROY_WITH_PARENT,
            GTK_MESSAGE_INFO,
            GTK_BUTTONS_OK,
            "Download Completed", "title");
  gtk_window_set_title(GTK_WINDOW(dialog), "Information");
  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
}

void show_error(GtkWidget *widget, gpointer window)
{
  GtkWidget *dialog;
  dialog = gtk_message_dialog_new(window,
            GTK_DIALOG_DESTROY_WITH_PARENT,
            GTK_MESSAGE_ERROR,
            GTK_BUTTONS_OK,
            "Error loading file");
  gtk_window_set_title(GTK_WINDOW(dialog), "Error");
  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
}

void show_question(GtkWidget *widget, gpointer window)
{
  GtkWidget *dialog;
  dialog = gtk_message_dialog_new(window,
            GTK_DIALOG_DESTROY_WITH_PARENT,
            GTK_MESSAGE_QUESTION,
            GTK_BUTTONS_YES_NO,
            "Are you sure to quit?");
  gtk_window_set_title(GTK_WINDOW(dialog), "Question");
  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
}

void show_warning(GtkWidget *widget, gpointer window)
{
  GtkWidget *dialog;
  dialog = gtk_message_dialog_new(window,
            GTK_DIALOG_DESTROY_WITH_PARENT,
            GTK_MESSAGE_WARNING,
            GTK_BUTTONS_OK,
            "Unallowed operation");
  gtk_window_set_title(GTK_WINDOW(dialog), "Warning");
  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
}

int main( int argc, char *argv[])
{

  GtkWidget *window;
  GtkWidget *table;

  GtkWidget *info;
  GtkWidget *warn;
  GtkWidget *que;
  GtkWidget *err;

  gtk_init(&argc, &argv);

  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
  gtk_window_set_default_size(GTK_WINDOW(window), 220, 150);
  gtk_window_set_title(GTK_WINDOW(window), "Message dialogs");

  table = gtk_grid_new();
  gtk_grid_insert_row (GTK_GRID(table),0);
  gtk_grid_insert_row (GTK_GRID(table),1);
  gtk_grid_insert_column (GTK_GRID(table),0);
  gtk_grid_insert_column (GTK_GRID(table),1);
  gtk_grid_set_row_homogeneous(GTK_GRID(table),TRUE);
  gtk_grid_set_column_homogeneous(GTK_GRID(table),TRUE);

  
  gtk_grid_set_row_spacing(GTK_GRID(table), 2);
  gtk_grid_set_column_spacing(GTK_GRID(table), 2);

  info = gtk_button_new_with_label("Info");
  warn = gtk_button_new_with_label("Warning");
  que = gtk_button_new_with_label("Question");
  err = gtk_button_new_with_label("Error");

  gtk_grid_attach(GTK_GRID(table), info, 0, 0, 1, 1);
  gtk_grid_attach(GTK_GRID(table), warn, 1, 0, 1, 1);
  gtk_grid_attach(GTK_GRID(table), que, 0, 1, 1, 1);
  gtk_grid_attach(GTK_GRID(table), err, 1, 1, 1, 1);
  
  gtk_container_add(GTK_CONTAINER(window), table);
  gtk_container_set_border_width(GTK_CONTAINER(window), 15);

  g_signal_connect(G_OBJECT(info), "clicked", 
        G_CALLBACK(show_info), (gpointer) window); 

  g_signal_connect(G_OBJECT(warn), "clicked", 
        G_CALLBACK(show_warning), (gpointer) window); 

  g_signal_connect(G_OBJECT(que), "clicked", 
        G_CALLBACK(show_question), (gpointer) window); 

  g_signal_connect(G_OBJECT(err), "clicked", 
        G_CALLBACK(show_error), (gpointer) window); 

  g_signal_connect_swapped(G_OBJECT(window), "destroy",
        G_CALLBACK(gtk_main_quit), G_OBJECT(window));

  gtk_widget_show_all(window);

  gtk_main();

  return 0;
}
