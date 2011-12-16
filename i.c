#include <winsock2.h>
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <assert.h>
#include <glib.h>

#define MEMSZ 10240000 /* 10M */
#define TELDEBUG 2

SOCKET sock;
char mem[MEMSZ];
int mempos;
int memlen;
FILE *outf;
char outfname[50];

struct si {
  double dx;
  double dy;
  int maxms;
  double minsens;
  double maxsens;
  double maxspeed;
  double maxturn;
  double maxhardturn;
} xI;

struct si *I = &xI;

typedef struct {
  int mils;
  char ctrls[2];
  double x;
  double y;
  double dir;
  double speed;
} Tel;

typedef struct Obj {
  struct Obj *next;
  char kind;
  double x;
  double y;
  double r;
  double mdir;
  double mspeed;
} Obj;

/* Temp speed calc */
int prevt;
double prevspeed;

void rawmemdump()
{
  int i;
  fprintf(outf, "\n\nBeginning raw dump\n");
  for(i = 0; i < memlen; i += 80)
  {
    fwrite("\n", 1, 1, outf);
    fwrite(mem + i, 1, memlen - i < 80 ? memlen - i : 80, outf);
  }
  fwrite("\n\n", 1, 2, outf);
  fclose(outf);
}

void sock_connect()
{
  struct sockaddr_in srv;
  srv.sin_family = AF_INET;
//  srv.sin_addr.s_addr = inet_addr("192.168.2.102");
  srv.sin_addr.s_addr = inet_addr("192.168.1.55");
  srv.sin_addr.s_addr = inet_addr("192.168.1.83");
//  srv.sin_addr.s_addr = inet_addr("192.168.2.105");
  srv.sin_addr.s_addr = inet_addr("10.0.0.100");
  srv.sin_port = htons(17676);
  sock = socket(AF_INET, SOCK_STREAM, 0);
  assert(sock != INVALID_SOCKET);
  assert(connect(sock, (SOCKADDR *)&srv, sizeof(srv)) == 0);
  assert(setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, "1", 1) == 0);
}

void read_data()
{
  int sz;
  assert(MEMSZ - mempos > 20);
  if(mem[mempos] == 'E')// || (mempos && mem[mempos] == 0))
    return;
  assert((sz = recv(sock, mem + mempos, MEMSZ - mempos, 0)) > 0);
  memlen += sz;
#if 0
  printf("sz = %4d     mempos = %4d    memlen = %4d\n", sz, mempos, memlen);
  printf("%.*s\n", sz, mem + mempos);
#endif
}

void read_init()
{
  char *begin = mem + mempos;
  char *end;
  read_data();
  assert(mem[mempos] == 'I');
  
  mempos++;
  I->dx = strtod(mem + mempos, &end);
  mempos = end - mem;
  
  mempos++;
  I->dy = strtod(mem + mempos, &end);
  mempos = end - mem;
  
  mempos++;
  I->maxms = strtol(mem + mempos, &end, 10);
  mempos = end - mem;
  mempos++;
  I->minsens = strtod(mem + mempos, &end);
  mempos = end - mem;
  
  mempos++;
  I->maxsens = strtod(mem + mempos, &end);
  mempos = end - mem;
  
  mempos++;
  I->maxspeed = strtod(mem + mempos, &end);
  mempos = end - mem;
  
  mempos++;
  I->maxturn = strtod(mem + mempos, &end);
  mempos = end - mem;
  
  mempos++;
  I->maxhardturn = strtod(mem + mempos, &end);
  mempos = end - mem;
  
  while(mem[mempos] != ';')
    mempos++;
  mempos++;

  fprintf(outf, "%.*s\n", end - begin, begin);
}

void read_tel(Tel *t)
{
  char *end;
  char *begin = mem + mempos;

  mempos += 2; /* "T " */
  t->mils = strtol(mem + mempos, &end, 10);
  mempos = end - mem;
  mempos++;

  memcpy(t->ctrls, mem + mempos, 2);
  mempos += 3;

  t->x = strtod(mem + mempos, &end);
  mempos = end - mem;
  mempos++;
  
  t->y = strtod(mem + mempos, &end);
  mempos = end - mem;
  mempos++;
  
  t->dir = strtod(mem + mempos, &end);
  mempos = end - mem;
  mempos++;
  
  t->speed = strtod(mem + mempos, &end);
  mempos = end - mem;
  mempos++;

#if TELDEBUG >= 1
  printf("%.*s\n", (mem + mempos) - begin, begin);
#endif
  fprintf(outf, "%.*s\n", (mem + mempos) - begin, begin);
}

void read_obj(Obj *o)
{
  char *end, *begin = (mem + mempos);
  o->kind = mem[mempos++];
  o->x = strtod(mem + mempos, &end);
  mempos = end - mem;
  mempos++;
  o->y = strtod(mem + mempos, &end);
  mempos = end - mem;
  mempos++;
  switch(o->kind)
  {
  case 'h':
  case 'b':
  case 'c':
    o->r = strtod(mem + mempos, &end);
    mempos = end - mem;
    mempos++;
    break;
  case 'm':
    o->mdir = strtod(mem + mempos, &end);
    mempos = end - mem;
    mempos++;
    o->mspeed = strtod(mem + mempos, &end);
    mempos = end - mem;
    mempos++;
    break;
  }
#if TELDEBUG >= 2
  printf("\t%.*s\n", (mem + mempos) - begin, begin);
#endif
  fprintf(outf, "\t%.*s\n", (mem + mempos) - begin, begin);
}

int read_end()
{
  char a;
  int t, s;
  char *end;
  a = mem[mempos];
  if(a != 'E')
  {
    mempos += 2;
    t = strtol(mem + mempos, NULL, 10);
    mempos = (strchr(mem + mempos, ' ') + 2) - mem;
#if TELDEBUG >= 2
    printf("===> %c %d next=%.*s\n", a, t, 10, mem + mempos);
#endif
    fprintf(outf, "===> %c %d\n", a, t);
    if(a == 'B')
    {
      //Sleep(2000);
      return 0;
    }
  }
  assert(mem[mempos] == 'E');
  a = mem[mempos];
  mempos += 2;
  t = strtol(mem + mempos, &end, 10);
  mempos = end - mem;
  mempos++;
  s = strtol(mem + mempos, &end, 10);
  mempos = end - mem;
  mempos++;
  assert(mem[mempos] == ';');
  mempos++;
  printf("End: %d -- score: %d\n", t, s);
  Sleep(3000);
  return 1;
}

int main()
{
  WORD ver = MAKEWORD(2, 2);
  WSADATA wsd;
  int err;
  time_t ltime;
  struct tm *ltm;
  err = WSAStartup(ver, &wsd);
  assert(err == 0);
  time(&ltime);
  ltm = localtime(&ltime);
  //sprintf(outfname, "Log_%s.txt", ctime(&ltime));
  sprintf(outfname, "Log_");
  strftime(outfname + 4, 46, "%Y%m%d_%H%M%S", ltm);
  strcat(outfname, ".txt");
  outf = fopen(outfname, "w");
  perror("f");
  assert(outf);
  atexit(rawmemdump);

  sock_connect();
  read_init();

  while(1)
  {
    Tel *t = malloc(sizeof(Tel));
    Obj *o = NULL, *obj, *obj2;
    if(memlen - mempos < 50)
      read_data();
    if(mem[mempos] != 'T')
    {
      //if(read_end())
	//read_init();
      read_end();
      continue;
    }
    read_tel(t);
    /* temp speed calc */
    {
#if 0
      st_ = max(st + (t_ - t)a - k(t_ - t)(st)^2, 0);
      st_ - st = (t_ - t)a - k(t_ - t)(st)^2;
      (st_ - st)/(t_ - t) = a - k(st)^2;
      (x - a)/(st^2) = k; x = (st_ - st)/(t_ - t);
#endif
      double k = (((t->speed - prevspeed)/(t->mils - prevt)) - (t->speed - prevspeed))/pow(prevspeed, 2.0);
      printf("Accel = %lf drag = %lf\n", t->speed - prevspeed, k);
      prevspeed = t->speed;
      prevt = t->mils;
    }
    /*******************/
    while(mem[mempos] != ';')
    {
      obj = malloc(sizeof(Obj));
      memset(obj, 0, sizeof(Obj));
      for(obj2 = o; obj2 && obj2->next; obj2 = obj2->next);
      if(o)
	obj2->next = obj;
      else
	o = obj;
      read_obj(obj);
    }
    mempos++;
/*     if(rand() > 20000) */
/*       send(sock, "r;", 2, 0); */
/*     else if(rand() > 2000) */
/*       send(sock, "l;", 2, 0); */

    if(t->mils < 100)
      send(sock, "a;", 2, 0);
    else if(t->mils == 100)
      send(sock, "b;", 2, 0);
/*     send(sock, "al;l;", 5, 0); */
/*     if(t->mils < 100) */
/*       send(sock, "a;", 2, 0);       */
/*     if(t->mils == 10000) */
/*       send(sock, "br;", 3, 0); */
  }
  
  WSACleanup();
}
