#include <gtk/gtk.h>
#include <glib.h>
#include <glib/gstdio.h>

#ifndef __CALC_H__
#define __CALC_H__

void __returnValue__(GtkWidget *label) {
FILE * popen(const char *command, const char *type);
int pclose(FILE *stream);

	gchar * result[1];
	FILE *file;
	FILE *fp;
	file = fopen("try.txt","w+"); 
	fprintf(file,"%s\n",gtk_label_get_text(GTK_LABEL(label))); 
	fclose(file); 
	if ((fp = popen("bc -l try.txt > out.txt", "w")) == NULL)
        {
                perror("pipe");
				return ;
        }
        pclose(fp);
        if(g_file_get_contents("out.txt", result, NULL, NULL)){
			gtk_label_set_text(GTK_LABEL(label), result[0]);
			}
 }
 #endif
