#include <gtk/gtk.h>
#include <string.h>

struct checkbuttons{
  int count;
  GtkWidget *label;
  GtkWidget **cbuttons;
};
 
void display(GtkWidget *widget, gpointer data)
{ 
  struct checkbuttons *temp = data;
  int i = 0;
  gchar str[256];
  strcpy(str, "");
  for(i = 0; i < temp->count; i++){
      if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON((temp->cbuttons)[i]))){
          gchar *text = gtk_button_get_label ((temp->cbuttons)[i]);
          strcat(str, text);
          strcat(str, "\n");
      }
  }
  gtk_label_set_text(GTK_LABEL(temp->label), str);
  
}

int main(int argc, char** argv){

    if (argc == 1){
        printf("At least one checkbutton should be specified\n");
        return 1;
    }
    GtkWidget *label;
    GtkWidget *window;
    GtkWidget *frame;
    GtkWidget *button;
    GtkWidget *cbuttons[argc-1];
    GtkWidget *buttonBox;
    
    gtk_init(&argc, &argv);
    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    
    frame = gtk_fixed_new();
    gtk_container_add(GTK_CONTAINER(window), frame);
    
    buttonBox = gtk_vbutton_box_new();
    /* create check buttons with input labels and packed in box*/
    int i;
    for(i = 1; i < argc; i++) {
      cbuttons[i-1] = gtk_check_button_new_with_label(argv[i]);
      gtk_container_add(GTK_CONTAINER(buttonBox), cbuttons[i-1]);
    }
    gtk_fixed_put(GTK_FIXED(frame), buttonBox, 50, 20);
    
    /* button clicked to display */
    button = gtk_button_new_with_label("click");
    gtk_fixed_put(GTK_FIXED(frame), button, 50, 120);
    
    /* label that display selected check button */
    label = gtk_label_new(" ");
    gtk_fixed_put(GTK_FIXED(frame), label, 190, 58);
    
    gtk_widget_show_all(window);
    struct checkbuttons data;
    data.label = label;
    data.cbuttons = cbuttons;
    data.count = argc -1;
    
    g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
    g_signal_connect(button, "clicked", G_CALLBACK(display), &data);
    
    gtk_main();
    
    return 0;
}