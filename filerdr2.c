#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
 
/*
 * static buffer to read a line, max of 1k bytes
 */
#define MAX_LINE_SIZE 1024
 
char linebuf[MAX_LINE_SIZE];
char * letters="abcdefghijklmnopqrstuvwxyz";
 
/*
* convert letter to number
*/
int lettertonumber(char arg)
{
	int i = 0;
	int k = 0;
	k = strlen(letters);
	while(i<k)
	{
		if(letters[i]==arg)
		{
			return i+1;
		}
		i++;
	}
	return -1;
}
 
/*
 * read a line from the file and print it out with 'id'
 * which should identify which partner this is
 */
void readline(pid_t id, FILE *file)
{
	char *e = fgets(linebuf, MAX_LINE_SIZE, file);
	int i =0,k=0;
	while(linebuf[i]!='\n' && linebuf[i]!='\0')
	{
		i++;
		k++;
		if(/*isalpha(linebuf[i]) &&*/ lettertonumber(linebuf[i])>9){
			printf("K ADD\n");
			k++;
		}		
	}
	/*
	* Allocate memory and copy line
	*/
	char * line = malloc((k)*sizeof(char));
	line[0]='\0';
	i=0;
	char * number=malloc(5*sizeof(char));
	char * ch = malloc(2*sizeof(char));
	ch[1]='\0';
	while(linebuf[i]!='\n' && linebuf[i]!='\0')
	{
		if(isalpha(linebuf[i]))
		{
			sprintf(number,"%d",lettertonumber(linebuf[i]));
			strcat(line,number);
		}
		else 
		{
			ch[0]=linebuf[i];
			strcat(line,ch);
		}
		i++;
	}
 
	if(e == NULL)
	{
		printf("[%d] *** NO LINE ***\n", id);
	}
	else
	{
 
		printf("%s %d\n",line,id);
		//printf("length: %d\n",strlen(line));
		fflush(stdout);
	}
	fflush(stdout);
}
 
 
/*
 * write the position of the file to the other partner
 */
void writeposition(off_t pos, int fdes)
{
	write(fdes, &pos, sizeof(off_t));
}
 
/*
 * read the position of the file from the other partner
 */
off_t readposition(int fdes)
{
	off_t pos;
	read(fdes, &pos, sizeof(off_t));
	return pos;
}
 
int main(int argc, char **argv)
{
	/* pipe to parent, pipe to child */
	int p_prnt[2], p_chld[2];
	/* default filename to read, source code of this program */
	char *filename = "FILE1";
	/* filehandle of file we are reading */
	FILE *file;
	/* process 'id', just a string of parent or child */
	char *id;
	/* pid used when seeing the fork result */
	int pid;
	/* pipe used in this process, assigned after fork */
	int mypipe[2];
	/* position of file, read from other partner */
	off_t pos;
	/* stat info, so we don't fseek past the end */
	struct stat finfo;
 
	/* all using a separate file if a name is given */
	if(argc > 1)
	{
		filename = argv[1];
	}
 
	/* init the pipes */
	if(pipe(p_prnt) || pipe(p_chld))
	{
		perror("pipe");
		exit(1);
	}
 
	/* open the file */
	if((file = fopen(filename, "r")) == NULL)
	{
		fprintf(stderr, "Failed to open '%s': %s\n",
				filename, strerror(errno));
		exit(1);
	}
 
	/* get the stat/size of the file */
	if(fstat(fileno(file), &finfo))
	{
		perror("fstat");
		exit(1);
	}
 
	/* fork and assign 'id' and 'mypipe' */
	if((pid = fork()) < 0)
	{
		perror("fork");
		exit(1);
	}
	else if(pid == 0)
	{
		id = "CHLD";
		mypipe[0] = p_chld[0];
		mypipe[1] = p_prnt[1];
		/*
		 * the child should wait until the parent reads the first line
		 * and then position itself for the loop to begin with the second line
		 */
		pos = readposition(mypipe[0]);
		fseek(file, pos, SEEK_SET);
	}
	else
	{
		id = "PRNT";
		mypipe[0] = p_prnt[0];
		mypipe[1] = p_chld[1];
	}
 
	/*
	 * main loop to
	 * 	read the line
	 * 	write the position to the other partner
	 * 	wait for the position from the other partner
	 * 		(it will read the next line in the mean-time)
	 * 	seek to the new position
	 */
	while(pos < finfo.st_size)
	{
		/* read */
		readline(getpid(), file);
		/* send pos */
		writeposition(ftell(file), mypipe[1]);
		/* get next */
		pos = readposition(mypipe[0]);
		/* seek */
		fseek(file, pos, SEEK_SET);
	}
 
	/* get other side to close down */
	writeposition(ftell(file), mypipe[1]);
 
	/* clean up and exit */
	close(mypipe[0]);
	close(mypipe[1]);
	fclose(file);
	return(0);
}

