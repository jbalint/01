/* $Id: mod_jelly.c,v 1.1 2005/12/05 23:34:18 jbalint Exp $ */

#include <sys/types.h>
#include <unistd.h>

#include "libpq-fe.h"

#include "httpd.h"
#include "http_config.h"
#include "http_log.h"
#include "http_core.h"
#include "ap_config.h"
#include "apr_hooks.h"

static void *create_dir_config(apr_pool_t *p, char *dirspec);
static void *merge_dir_config(apr_pool_t *p,
		void *parent_conf, void *newloc_conf);
static const char *cmd_jelly(cmd_parms *cmd, void *mconfig);
static void register_hooks(apr_pool_t *p);
static int post_config(apr_pool_t *p, apr_pool_t *plog,
		apr_pool_t *ptemp, server_rec *s);
static int access_checker(request_rec *r);

static void dbsetup();
static void dbteardown();

static PGconn *conn = NULL;
static char *connspec = "dbname=scratch02 host=stephenw.improvedideas.com user=jbalint password=xxx";

typedef struct {
	int checkAccess;
} jelly_dir_config;

static const command_rec jelly_cmds[] =
{
	AP_INIT_NO_ARGS(
		"JellyAccessControl",
		cmd_jelly,
		NULL,
		OR_OPTIONS,
		"Example directive - no arguments"
	),
	{NULL}
};

module AP_MODULE_DECLARE_DATA jelly_module =
{
	STANDARD20_MODULE_STUFF,
	create_dir_config,
	merge_dir_config,
	NULL,
	NULL,
	jelly_cmds,
	register_hooks,
};

static void *create_dir_config(apr_pool_t *p, char *dirspec)
{
	jelly_dir_config *cfg = apr_pcalloc(p, sizeof(jelly_dir_config));
	cfg->checkAccess = 0;
	/*
	fprintf(stderr, "jelly: setting up for %s\n", dirspec);
	*/
	return (void *)cfg;
}

static void *merge_dir_config(apr_pool_t *p,
		void *parent_conf, void *newloc_conf)
{
	jelly_dir_config *pconf = (jelly_dir_config*)parent_conf;
	jelly_dir_config *nconf = (jelly_dir_config*)newloc_conf;
	if(nconf->checkAccess)
	{
		return newloc_conf;
	}
	else
	{
		return parent_conf;
	}
}

static const char *cmd_jelly(cmd_parms *cmd, void *mconfig)
{
	jelly_dir_config *jconf = (jelly_dir_config *)mconfig;
	jconf->checkAccess = 1;
	return NULL;
}

static void register_hooks (apr_pool_t *p)
{
	ap_hook_post_config(post_config, NULL, NULL, APR_HOOK_MIDDLE);
	ap_hook_access_checker(access_checker, NULL, NULL, APR_HOOK_MIDDLE);
}

static int post_config(apr_pool_t *p, apr_pool_t *plog,
		apr_pool_t *ptemp, server_rec *s)
{
	/*
	fprintf(stderr,"jelly: setting up. pid=%d\n", getpid());
	*/
	return OK;
}

static int access_checker(request_rec *r)
{
	PGresult *res;
	int rc = HTTP_FORBIDDEN;
	char *pvals[1];
	int isip = 0;
	jelly_dir_config *conf;

	conf = (jelly_dir_config *)
		ap_get_module_config(r->per_dir_config, &jelly_module);

	if(conf == NULL)
	{
		/*
		fprintf(stderr, "jelly: config null for '%s'\n", r->path_info);
		fflush(stderr);
		*/
		return OK;
	}

	if(!conf->checkAccess)
	{
		/*
		fprintf(stderr, "jelly: config says to skip for '%s'\n", r->path_info);
		fflush(stderr);
		*/
		return OK;
	}

	/*
	fprintf(stderr, "jelly: checking for '%s'\n", r->path_info);
	fflush(stderr);
	*/

	pvals[0] = (char *)ap_get_remote_host(r->connection,
			r->per_dir_config, REMOTE_NAME, &isip);

	if(!isip)
	{
		/* TODO: get the IP addr here and build a string */
		fprintf(stderr, "jelly: didn't get ip for remote host '%s'\n",
				pvals[0]);
		return OK;
	}

	dbsetup();

	/* default to forbidden if there is no connection */
	if(conn == NULL)
	{
		/* TODO: add host to this message? */
		fprintf(stderr, "jelly: No db connection, default FORBIDDEN\n");
		return HTTP_FORBIDDEN;
	}

	res = PQexecParams(conn, "select count(*) from apache_auth "
			" where cleared = true and ipaddr = $1",
			1, NULL, (const char *const *)pvals, NULL, NULL, 0);
	if(PQresultStatus(res) != PGRES_TUPLES_OK)
	{
		fprintf(stderr, "jelly: Auth check query failed: %s\n",
				PQresultErrorMessage(res));
		PQclear(res);
		return HTTP_FORBIDDEN;
	}

	if(!strcmp("1", PQgetvalue(res, 0, 0)))
	{
		rc = OK;
	}

	PQclear(res);

	return rc;
}

static int setupRecurse = 0;
static void dbsetup()
{
	PGresult *res;

	setupRecurse = 1;

	/* First-time initial setup */
	if(conn == NULL)
	{
		/* TODO: somehow better message */
		/*
		fprintf(stderr, "jelly: making db connection in pid %d\n", getpid());
		fflush(stderr);
		*/
		conn = PQconnectdb(connspec);

		if(PQstatus(conn) != CONNECTION_OK)
		{
			fprintf(stderr, "jelly: failed to connect to database: %s\n",
					PQerrorMessage(conn));
			PQfinish(conn);
			conn = NULL;
			return;
		}

		if(atexit(dbteardown) != 0)
		{
			fprintf(stderr, "jelly: failed to register atexit() handler\n");
		}
	}

	/* test the connection */
	res = PQexec(conn, "select 1");
	if(PQresultStatus(res) != PGRES_TUPLES_OK)
	{
		PQclear(res);
		fprintf(stderr, "jelly: connection lost: %s\n",
				PQresultErrorMessage(res));
		PQfinish(conn);
		conn = NULL;
		if(setupRecurse)
		{
			fprintf(stderr, "jelly: reconnected and test failed\n");
		}
		else
		{
			dbsetup();
		}
	}
	else
	{
		PQclear(res);
	}

	setupRecurse = 0;
}

static void dbteardown()
{
	/* TODO: remove this message */
	/*
	fprintf(stderr, "jelly: closing!!\n");
	*/
	PQfinish(conn);
	conn = NULL;
}

