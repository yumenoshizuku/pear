#include <gtk/gtk.h>
#include <string.h>

struct radiobuttons{
    int count;
    GtkWidget *label;
    GtkWidget **rbuttons;
};
void display(GtkWidget *widget, gpointer data)
{ 
  struct radiobuttons *temp = data;
  gchar *str;
  int i = 0;
  for(i = 0; i < temp->count; i++){
      if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON((temp->rbuttons)[i]))){
          str = gtk_button_get_label ((temp->rbuttons)[i]);
          gtk_label_set_text(GTK_LABEL(temp->label), str);
      } 
  }
  gtk_label_set_text(GTK_LABEL(temp->label), str);
}
  
int main(int argc, char** argv){

    if (argc == 1){
        printf("At least one radiobutton should be specified\n");
        return 1;
    }
    GtkWidget *label;
    GtkWidget *window;
    GtkWidget *frame;
    GtkWidget *button;
    GtkWidget *rbuttons[argc-1];
    GtkWidget *buttonBox;
    
    gtk_init(&argc, &argv);
    window = gtk_window_new(GTK_WINDOW_TOPLEVEL); 
    frame = gtk_fixed_new();
    gtk_container_add(GTK_CONTAINER(window), frame);
    buttonBox = gtk_vbutton_box_new();
    
    /* create radio buttons with input labels and packed into a box*/   
    rbuttons[0] = gtk_radio_button_new_with_label(NULL, argv[1]);
    gtk_container_add(GTK_CONTAINER(buttonBox), rbuttons[0]);
    int i;
    for(i = 2; i < argc; i++) {
      rbuttons[i-1] = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON (rbuttons[0]),
      															  argv[i]);
      gtk_container_add(GTK_CONTAINER(buttonBox), rbuttons[i-1]);
    }
    gtk_fixed_put(GTK_FIXED(frame), buttonBox, 50, 20);
    
       /* button clicked to display */
    button = gtk_button_new_with_label("click");
    gtk_fixed_put(GTK_FIXED(frame), button, 50, 120);
    
    /* label that display selected check button */
    label = gtk_label_new(" ");
    gtk_fixed_put(GTK_FIXED(frame), label, 190, 58);
    gtk_widget_show_all(window);
    
    /* prepare the data to pass as arguement for function diplay */
    struct radiobuttons data;
    data.label = label;
    data.rbuttons = rbuttons;
    data.count = argc -1;
    
    g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
    g_signal_connect(button, "clicked", G_CALLBACK(display), &data);   
    
    gtk_main();
    
    return 0;
}