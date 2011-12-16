#include <iostream>

using namespace std;

struct treenode
{
    struct treenode *child;
    struct treenode *nextsib;
    char *firstchar;
    char *lastchar;

    treenode() : child(NULL), nextsib(NULL),
	firstchar(NULL), lastchar(NULL) {};
    treenode(char *fc, char *lc) : child(NULL), nextsib(NULL)
    	firstchar(fc), lastchar(lc) {};
};

treenode *build_suffix_tree(char *str, int len)
{
    int i, j;
    treenode *root = new treenode;
    treenode *first = new treenode(str, str);
    root->child = first;
    for(i = 1; i <= len; ++i)
    {
	j = 0;

	/* TODO how do you know if this is linked to the root node? */
    }
    return root;
}

void print_tree(treenode *tr, char *str, int indent)
{
    char f = ' ', l = ' ';
    if(tr->firstchar)
	f = *tr->firstchar;
    if(tr->lastchar)
	l = *tr->lastchar;
    for(int i = 0; i < indent; ++i)
	printf("-");
    printf("N(%d,%d)(%c..%c)\n", tr->firstchar, tr->lastchar, f, l);
    if(tr->child != NULL)
	print_tree(tr->child, str, indent + 1);
    if(tr->nextsib != NULL)
	print_tree(tr->nextsib, str, indent);
}

int main (int argc, char *argv[])
{
    char *str = "asdbdndn";
    printf("Building suffix tree for '%s'\n", str);
    treenode *tr = build_suffix_tree(str, strlen(str));
    print_tree(tr, str, 0);
    delete tr;
    /*                                                              
    treenode *tr = new treenode;
    tr->child = new treenode;
    tr->nextsib = new treenode;
    tr->firstchar = str + 2;
    tr->lastchar = str + 4;
    print_tree(tr, str, 0);
    */
    return(0);
}

