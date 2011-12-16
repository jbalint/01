#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/mman.h>

#define BUFSZ (1024*1024)
/* shared mem */
char *buf;
ssize_t *sz;
int *cnt;

/* child (writer) vars */
int fd;
pid_t pid;

/* empty func to make sure pause returns in parent */
void ack_sig(int sig) {
}

/* children */
void write_data(int sig) {
	int i = *sz;
	while (i > 0)
		i -= write(fd, buf, i);
	(*cnt)--;
	kill(pid, SIGUSR2);
}

void close_file(int sig) {
	close(fd);
	exit(1);
}

int main(int argc, char **argv) {
	struct sigaction sa;
	int i;

	argc--; argv++;
	if (argc < 1)
		return 0;

	/* allocate the shared memory of buffer, buf size, and sync obj */
	buf = mmap(NULL, BUFSZ + sizeof(ssize_t) + sizeof(int),
	           PROT_READ|PROT_WRITE, MAP_SHARED|MAP_ANONYMOUS, 0, 0);
	sz = (ssize_t*)(buf+BUFSZ);
	cnt = (int*)(buf+BUFSZ+sizeof(ssize_t));
	*cnt = argc;
	setsid(); /* make sure we have our own process group for kill(0, ...) signals */
	memset(&sa, 0, sizeof(struct sigaction));
	sa.sa_flags = 0
	sigemptyset(&sa.sa_mask);
	sigaddset(&sa.sa_mask, SIGUSR1);
	sigaddset(&sa.sa_mask, SIGUSR2);
	sa.sa_handler = ack_sig;
	sigaction(SIGUSR1, &sa, NULL);
	sigaction(SIGUSR2, &sa, NULL);

	for (i = 0; i < argc; ++i, ++argv) {
		if (!(pid = fork())) {
			sa.sa_handler = write_data;
			sigaction(SIGUSR1, &sa, NULL);
			sa.sa_handler = close_file;
			sigaction(SIGUSR2, &sa, NULL);
			pid = getppid();
			fd = open(*argv, O_WRONLY|O_CREAT|O_TRUNC, 0666);
			(*cnt)--;
			kill(pid, SIGUSR2);
			for (;;)
				pause();
		}
	}

	do {
		/* wait until all child processes have sync'd before the next read */
		while (*cnt > 0)
			pause();
		*sz = read(STDIN_FILENO, buf, BUFSZ);
		*cnt = argc;
		kill(0, SIGUSR1);
	} while (*sz);
	kill(0, SIGUSR2);
	return 0;
}
