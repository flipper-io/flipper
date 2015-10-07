#include <flipper.h>

#include <unistd.h>

void progress(int x, int t, int r, int w) {
    
    if (x % (t / r + 1) != 0) return;
    
    float ratio = (float)(((float)(x)) / ((float)(t))) + 0.01;
    
    int c = ratio * w;
    
    printf("%3d%% | ", (int)(ratio * 100));
    
    for (int x=0; x < c; x ++) printf("#");
    
    for (int x = c; x < w; x ++) printf(" ");
    
    printf(" |\n");
    
    if (ratio < 0.9999f) printf("\033[F\033[J");
    
}

void wait_with_progress(int seconds) {
    
    for (int i = 0; i < (seconds * 500); i ++) {
        
        progress(i, (seconds * 500), 100, 60);
        
        usleep(1000);
        
    }
    
}