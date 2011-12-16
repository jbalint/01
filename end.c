#include <stdio.h>
#include <unistd.h>

#include <assert.h>

static void checkit(void* ptr)
{
	void* endds = sbrk(0);
	assert(endds != (void*)-1);
	if(ptr < endds)
		printf("heap (%x < %x)", ptr, endds);
	else
		printf("stack (%x >= %x)", ptr, endds);
}

int main(int argc, char** argv)
{
	extern int _etext;
	extern int _edata;
	extern int _end;
	int stack;
	int stack2;
	int* heap;
	heap = (int*)malloc(4);
	/* why all of a sudden is this causing a bus error!? */
	/*
	printf("etext = %x\n", _etext);
	*/
	printf("edata = %x\n", _edata);
	printf("end   = %x\n", _end);
	malloc(1);
	printf("end   = %x\n", _end);
	malloc(1);
	printf("end   = %x\n", _end);
	malloc(1);
	printf("end   = %x\n", _end);
	malloc(1);
	printf("end   = %x\n", _end);
	printf("sbrk  = %x\n", sbrk(0));
	printf("edata = %x\n", _edata);

	printf("stack variable detected as: ");
	checkit(&stack);
	printf("\n");
	printf("stack2 variable detected as: ");
	checkit(&stack2);
	printf("\n");
	printf("heap variable detected as: ");
	checkit(heap);
	printf("\n");
}
