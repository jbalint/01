#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char** argv)
{
	int megs = 4096;
	int increment = 100;
	int i;
	int j;
	void* memx[1000];

	for(i = 0; i < megs; i += increment)
	{
		printf("i=%d Heap pointer '%08x' ", i, sbrk(0));
		if((memx[i/increment] = malloc(increment * 1024 * 1024)) == (void*)-1)
		{
			perror("malloc()");
			exit(1);
		}
		*((char*)memx[i/increment]) = 1;
		memset(memx[i/increment], 'x', increment * 1024 * 1024);
		/*
		*/
		printf("Heap pointer '%08x'\n", sbrk(0));
		fflush(stdout);

		for(j = 0; j < 40000000; ++j)
			j = 1 + j - 1;
	}

}
