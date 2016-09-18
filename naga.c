/* utility for dealing with Razar Naga extra buttons */
// https://www.kernel.org/doc/Documentation/input/input.txt
// https://www.kernel.org/doc/Documentation/input/event-codes.txt
// Disable keyboard events reaching X: xinput --disable `xinput --list --id-only "keyboard:Razer Razer Naga Chroma"`
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <linux/input.h>

int main(int argc, char **argv)
{
    struct input_event ev;
    int fd;
    int sz;
    if ((fd = open("/dev/input/by-id/usb-Razer_Razer_Naga_Chroma-if02-event-kbd", O_RDONLY)) < 0)
    {
        perror("Failed to open");
        return 1;
    }
    while ((sz = read(fd, &ev, sizeof(struct input_event))) > 0)
    {
        if (ev.type == EV_KEY) {
            /* ev.code will be KEY_1-KEY_EQUAL (2-13) */
            printf("Key: %d\n", ev.code);
        }
    }
    perror("Failed to read");
    return 1;
}
