#include "stdio.h"
#include "stdlib.h"


typedef struct Node {
  int val;
  struct Node *next;
} Node;

typedef struct List {
  Node* root;
  int length;
} List;

typedef struct DNode {
  int val;
  struct DNode *prev;
  struct DNode *next;
} DNode;

List* genlist(int length, int (*f)(int)){
  Node *root = malloc(sizeof(Node));
  Node *c = root;
  for(int i = 0; i < length; i++){
	Node *n = malloc(sizeof(Node));
	n->val = f(i);
	c->next = n;
	c = c->next;
	c->next = root->next;
  }
  List *l = malloc(sizeof(List));
  l->root = root;
  l->length = length;
  return l;
}

int valgen(int i){
  return i; /* rand() % 100; */
}




/* gendlist(int length, int max){ */
/*   srand(42); */
/*   DNode *root = malloc(sizeof(DNode)); */
/*   Node *c = root; */
  

int main(int argc, char **argv){
  List* l = genlist(10, *valgen);
  Node* c = l->root->next;
  for(int i = 1; i <= 3*l->length; i++){
	printf("%i ", c->val);
	c = c->next;
	if(i % l->length == 0){ printf("\n"); }
  }

  return 0;
}
