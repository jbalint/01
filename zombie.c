
/*
 * $Id: zombie.c 2 2007-07-26 03:52:55Z jbalint $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <curses.h>
#include <signal.h>
#include <assert.h>
#include <sys/time.h>
#include <sys/poll.h>

/*
 * watch -n 1 "ps -ef | grep -i defunct | grep -v cscope | grep -v grep"
 * cc -Wall -ggdb3 -o zOmBiE zombie.c -lcurses && ./zOmBiE
 */

/*
 * http://web.cs.mun.ca/~rod/ncurses/ncurses.html
 */

typedef struct l_l {
	struct l_l *n;
	pid_t pid;
	int ttm;
	int x;
	int y;
	char i;
	char b[20];
	int bl;
} list;

static char *acs = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ~!@#$%^&*()_+`-=1234567890[]\\';/.,?><:\"{}|";
static int acsl;

static struct pollfd infd;
static struct itimerval itimerval;

static int redraw = 0;
/* number of counts before adding a new <XXX> */
#define COUNTSTOADD 10
#define COUNTSTOMOVE 6
static int countsleft = COUNTSTOADD;
static list *zs = NULL;
static const int boff = 10;

void update(int x __attribute__((unused)))
{
	redraw = 1;
}

void randcoords(int *x, int *y)
{
	*x = rand() % (curscr->_maxx - 10);
	*y = rand() % (curscr->_maxy - 00);
}

void randid(char *c)
{
	*c = acs[rand() % acsl];
}

void drawt()
{
	int i;
	attron(COLOR_PAIR(COLOR_RED));
	for(i = 0; i < 3; ++i)
	{
		addch('~');
	}
	attroff(COLOR_PAIR(COLOR_RED));
}

void drawc(char c)
{
	attron(COLOR_PAIR(COLOR_YELLOW));
	addch(c);
	attroff(COLOR_PAIR(COLOR_YELLOW));
}

void clearz(list *z)
{
	int i;
	move(z->y, z->x);
	for(i = 0; i < z->bl + boff; ++i)
	{
		addch(' ');
	}
}

void newz()
{
	list *z = malloc(sizeof(list));
	z->n = NULL;
	if(zs)
		z->n = zs;
	randcoords(&z->x, &z->y);
	randid(&z->i);
	z->ttm = COUNTSTOMOVE;
	if(!(z->pid = fork()))
		exit(0);
	zs = z;
	sprintf(z->b, "%d", z->pid);
	z->bl = strlen(z->b);
}

int main(int argc, char **argv)
{
	int blah = 5;
	initscr();
	keypad(stdscr, TRUE);
	nonl();
	cbreak();
	noecho();

	acsl = strlen(acs);

	srand(time(0));

	start_color();
	init_pair(COLOR_RED, COLOR_RED, COLOR_BLACK);
	init_pair(COLOR_GREEN, COLOR_GREEN, COLOR_BLACK);
	init_pair(COLOR_YELLOW, COLOR_YELLOW, COLOR_BLACK);

	/* ******************************* */
	/* init some <XXX>'s */
	while(blah-- > 0)
	{
		newz();
	}
	/* ******************************* */

	infd.fd = 0;
	infd.events = POLLIN;

	/* set up timer */
	assert(signal(SIGALRM, update) != SIG_ERR);

	itimerval.it_value.tv_sec = 0;
	itimerval.it_value.tv_usec = 500;
	itimerval.it_interval.tv_sec = 0;
	itimerval.it_interval.tv_usec = 100000;
	/*
	itimerval.it_interval.tv_usec = 500000;
	*/

	assert(!setitimer(ITIMER_REAL, &itimerval, NULL));

	while(1)
	{
		list *z;

		poll(&infd, 1, 1000);
		if(infd.revents)
		{
			list *p;
			char c;
			assert(read(0, &c, 1) == 1);

			p = z = zs;
			while(z)
			{
				if(z->i == c)
				{
					int i;

					waitpid(z->pid, &i, 0);

					clearz(z);
					if(z == zs)
						zs = z->n;
					else
						p->n = z->n;
					free(z);
					break;
				}
				p = z;
				z = z->n;
			}

			if(!zs)
			{
				endwin();
				printf("YOU WON!!!!!!!!!!!\n");
				exit(1);
			}
		}

		if(redraw)
		{
			z = zs;
			while(z)
			{
				int i;

				clearz(z);

				move(z->y, z->x);

				if(--z->ttm == 0)
				{
					randcoords(&z->x, &z->y);
					move(z->y, z->x);
					z->ttm = COUNTSTOMOVE;
					/* TODO not working? */
				}

				drawc(z->i);
				drawt();

				attron(COLOR_PAIR(COLOR_GREEN));
				for(i = 0; i < z->bl; ++i)
				{
					addch(z->b[i]);
				}
				attroff(COLOR_PAIR(COLOR_GREEN));

				drawt();
				drawc(z->i);

				z = z->n;
			}

			if(countsleft-- == 0)
			{
				newz();
				countsleft = COUNTSTOADD;
			}

			redraw = 0;
		}

		move(curscr->_maxy, curscr->_maxx);
		refresh();
	}

	endwin();

	return 0;
}

