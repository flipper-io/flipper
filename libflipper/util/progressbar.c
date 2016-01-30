#include <progressbar.h>
#include <stdio.h>

void print_progress(int progress, int total_len, int char_width)
{
	if (total_len == 0)
	{
		return;
	}
	float ratio = (progress / ((float)total_len));
	
	int bar_width_calculated = ratio * char_width;
	
	printf("%3d%% | ", (int)(ratio * 100));
	
	for (int i = 0; i < char_width; i++)
	{
		printf((i < bar_width_calculated) ? "#" : " ");
	}
	
	printf(" |\n");
	
	if (ratio < 0.9999f) printf("\033[F\033[J");
}