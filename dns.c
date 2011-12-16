
/*
 * very simple DNS server, for experimentation
 *
 * protocol info from http://www.freesoft.org/CIE/RFC/1035/40.htm
 * udp modeled from http://www.cs.rpi.edu/courses/sysprog/sockets/server_udp.c
 *
 * cc -o dnsexpirement dns.c -lnsl -lsocket
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

/* DNS header flag shiftbits and masks */
#define FM_QR (1<<15) /* query (0) or response (1) */
#define FM_OC (0xff<<11) /* opcode, 0=QUERY,1=IQUERY,2=STATUS,else undefined */
#define FM_AA (1<<10) /* is authoritative answer? */
#define FM_TC (1<<9) /* truncated? */
#define FM_RD (1<<8) /* recursion desired? */
#define FM_RA (1<<7) /* recursion available? */
#define FM_Z (0x7f<<4) /* zero */
#define FM_RC (0xff) /* response code, 0=ok,1=format err,2=server fail,
						3=name err,4=not impl,5=refused,else undefined */

#define B_QR 15
#define B_OC 11
#define B_AA 10
#define B_TC  9
#define B_RD  8
#define B_RA  7
#define B_RC  0

/*
 * header part of the structure
 */
typedef struct {
	uint16_t id; /* transaction id */
	uint16_t flags; /* flags, see masks above */
	uint16_t qdcnt; /* # of questions */
	uint16_t ancnt; /* # of resource records */
	uint16_t nscnt; /* # of ns resources in authority section */
	uint16_t arcnt; /* # of additional resources */
} dnshdr;

/*
 * question/query data
 */
typedef struct {
	char qname[100];
	uint16_t qtype;
	uint16_t qclass;
} dnsquest;

/*
 * resource/response data
 */
typedef struct {
	union {
		/*char *name;*//*labels*/
		uint16_t offset;
	} qname;
	/*char qname_t;*/
	uint16_t type;
	uint16_t class;
	uint32_t ttl;
	uint16_t rdlength;
	/* usually 4byte IP or 2byte IDX */
	char rdata[10];
} dnsrsrc;

/*
 * request structure contains a header and array of questions
 */
typedef struct {
	dnshdr hdr;
	dnsquest *quests;
} dnsreq;

typedef struct {
	dnshdr hdr;
	dnsquest *quests;
	dnsrsrc *answers;
	dnsrsrc *authorities;
	dnsrsrc *additionals;
} dnsresp;

/*
 * parse a query in buf into the structure req
 */
int parsequery(char *buf, dnsreq *req)
{
	int i,j;
	dnsquest *q;
	char *qn;

	/* header data is simple */
	req->hdr.id = ntohs(*(uint16_t*)buf);
	buf += 2;
	req->hdr.flags = ntohs(*(uint16_t*)buf);
	buf += 2;
	req->hdr.qdcnt = ntohs(*(uint16_t*)buf);
	buf += 2;
	req->hdr.ancnt = ntohs(*(uint16_t*)buf);
	buf += 2;
	req->hdr.nscnt = ntohs(*(uint16_t*)buf);
	buf += 2;
	req->hdr.arcnt = ntohs(*(uint16_t*)buf);
	buf += 2;

	/* allocate array for questions */
	if(req->hdr.qdcnt)
	{
		req->quests = malloc(sizeof(dnsquest) * req->hdr.qdcnt);
		memset(req->quests, 0, sizeof(dnsquest) * req->hdr.qdcnt); 
	}

	/* read the questions */
	for(i = 0; i < req->hdr.qdcnt; ++i)
	{
		q = &req->quests[i];
		qn = q->qname; /* pointer to move along with added labels */
		/* read the labels */
		for(j = *(uint8_t*)buf++; j; )
		{
			strncat(qn, buf, j);
			buf += j;
			qn += j;
			j = *(uint8_t*)buf++;
			if(j) strcat(qn, ".");
		}

		/* other question data */
		q->qtype = ntohs(*(uint16_t*)buf);
		buf += 2;
		q->qclass = ntohs(*(uint16_t*)buf);
		buf += 2;
	}

	return 0;
}

/*
 * convert a dot-separated qname into a set of labels
 */
int stringtolabels(char *buf, char *name)
{
	char *orig = buf;
	char i = 0;
	for(; *name; ++name, ++i)
	{
		if(*name == '.')
		{
			*buf++ = i;
			memcpy(buf, name - i, i);
			buf += i;
			i = -1;
		}
	}
	*buf++ = i;
	memcpy(buf, name - i, i);
	buf += i;
	*buf++ = 0;
	return buf - orig;
}

/*
 * serialize a dns resource into the output buffer
 */
int serialzrsrc(char *buf, dnsrsrc *r)
{
	char *orig = buf;
	*(uint16_t*)buf = htons(r->qname.offset);
	buf += 2;
	*(uint16_t*)buf = htons(r->type);
	buf += 2;
	*(uint16_t*)buf = htons(r->class);
	buf += 2;
	*(uint32_t*)buf = htonl(r->ttl);
	buf += 4;
	*(uint16_t*)buf = htons(r->rdlength);
	buf += 2;
	memcpy(buf, r->rdata, r->rdlength);
	buf += r->rdlength;
	return buf - orig;
}

/*
 * serialize the response structure into the output buffer
 */
int serialzresponse(char *buf, dnsresp *resp)
{
	char *orig = buf;

	/* header */
	*(uint16_t*)buf = htons(resp->hdr.id);
	buf += 2;
	*(uint16_t*)buf = htons(resp->hdr.flags);
	buf += 2;
	*(uint16_t*)buf = htons(resp->hdr.qdcnt);
	buf += 2;
	*(uint16_t*)buf = htons(resp->hdr.ancnt);
	buf += 2;
	*(uint16_t*)buf = htons(resp->hdr.nscnt);
	buf += 2;
	*(uint16_t*)buf = htons(resp->hdr.arcnt);
	buf += 2;

	/* questions */
	buf += stringtolabels(buf, resp->quests[0].qname);
	*(uint16_t*)buf = htons(resp->quests[0].qtype);
	buf += 2;
	*(uint16_t*)buf = htons(resp->quests[0].qclass);
	buf += 2;

	/* answers */
	buf += serialzrsrc(buf, &resp->answers[0]);

	/* authorities */
	buf += serialzrsrc(buf, &resp->authorities[0]);

	return buf - orig;
}

/*
 * print a header
 */
void printdnshdr(dnshdr *hdr)
{
	printf("** hdr id=0x%02hx\n", hdr->id);
	printf("flags: 0x%04hx\n", hdr->flags);
	printf(" QR=%d\n",		(hdr->flags & FM_QR) >> B_QR);
	printf(" OC=0x%04hx\n",	(hdr->flags & FM_OC) >> B_OC);
	printf(" AA=%d\n",		(hdr->flags & FM_AA) >> B_AA);
	printf(" TC=%d\n",		(hdr->flags & FM_TC) >> B_TC);
	printf(" RD=%d\n",		(hdr->flags & FM_RD) >> B_RD);
	printf(" RA=%d\n",		(hdr->flags & FM_RA) >> B_RA);
	printf(" RC=0x%04hx\n",	(hdr->flags & FM_RC) >> B_RC);
	printf("cnt qd=%hd, an=%hd, ns=%hd, ar=%hd\n",
			hdr->qdcnt, hdr->ancnt, hdr->nscnt, hdr->arcnt);
}

/*
 * print a question
 */
void printdnsquest(dnsquest *q)
{
	printf("** question\n");
	printf("qname=%s\n", q->qname);
	printf("qtype=0x%04hx\n", q->qtype);
	printf("qclass=0x%04hx\n", q->qclass);
}

int main(int argc, char **argv)
{
	int sock;
	struct sockaddr_in srvr;

	/* open and listen on the UDP standard port 53 (needs root) */
	if((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
	{
		perror("socket");
		exit(1);
	}

	memset(&srvr, 0, sizeof(srvr));
	srvr.sin_family = AF_INET;
	srvr.sin_addr.s_addr = INADDR_ANY;
	srvr.sin_port = 53;

	if(bind(sock, (struct sockaddr *)&srvr, sizeof(srvr)) < 0)
	{
		perror("bind");
		exit(1);
	}

	/* main loop accept requests */
	while(1)
	{
		int i;
		char outbuf[512];
		char inbuf[512];
		struct sockaddr_in clntaddr;
		int clntaddrlen = sizeof(clntaddr);
		int sz = recvfrom(sock, inbuf, 1024, 0,
				(struct sockaddr *)&clntaddr, &clntaddrlen);
		dnsreq req;
		dnsresp resp;

		if(sz < 0)
		{
			perror("recvfrom");
			exit(1);
		}

		if(parsequery(inbuf, &req))
		{
			fprintf(stderr, "Bad query\n");
			if(req.hdr.qdcnt) free(&req.quests);
			continue;
		}

		/* print out requests */
		printf("----------------------------------\n");
		printdnshdr(&req.hdr);
		for(i = 0; i < req.hdr.qdcnt; ++i)
			printdnsquest(&req.quests[i]);

		/* create the response */
		/* copy the header and then change it */
		memset(&resp, 0, sizeof(resp));
		memcpy(&resp.hdr, &req.hdr, sizeof(req.hdr));
		resp.hdr.ancnt = 1;
		resp.hdr.nscnt = 1;
		resp.hdr.flags |= (1<<B_QR) | (1<<B_AA);
		resp.quests = malloc(sizeof(dnsquest));
		/* copy the question */
		memcpy(resp.quests, req.quests, sizeof(dnsquest));
		/* create answer */
		resp.answers = malloc(sizeof(dnsrsrc));
		resp.answers[0].qname.offset = 0xc00c; /* XXX magic numbers ;) */
		resp.answers[0].type = 1;
		resp.answers[0].class = 1;
		resp.answers[0].ttl = 0x15810;
		resp.answers[0].rdlength = 4;
		*(uint32_t*)resp.answers[0].rdata = htonl(0x0a000054);
		/* create authority */
		resp.authorities = malloc(sizeof(dnsrsrc));
		resp.authorities[0].qname.offset = 0xc00c;
		resp.authorities[0].type = 2;
		resp.authorities[0].class = 1;
		resp.authorities[0].ttl = 0x15810;
		resp.authorities[0].rdlength = 2;
		*(uint16_t*)resp.authorities[0].rdata = htons(0xc00c);

		/* free the original questions */
		if(req.hdr.qdcnt) free(&req.quests);

		/* build response buffer */
		sz = serialzresponse(outbuf, &resp);

		/* send response packet */
		if((sz = sendto(sock, outbuf, sz, 0, (struct sockaddr *)&clntaddr,
						clntaddrlen)) < 0)
		{
			perror("sendto");
			exit(1);
		}

		/* free the response data */
		free(resp.quests);
		free(resp.answers);
		free(resp.authorities);
	}
}

