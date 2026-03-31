#if defined(__linux__) || defined(__APPLE__)

#include <sys/ioctl.h>

int aiskills_get_terminal_width() {
    struct winsize ws;
    if (ioctl(0, TIOCGWINSZ, &ws) == 0) {
        return ws.ws_col;
    }
    return -1;
}

#endif
