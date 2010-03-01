#include "stdio.h"
#include "stdlib.h"


typedef struct Node {
  int val;
  struct Node *next;
} Node;

typedef struct DNode {
  int val;
  struct DNode *prev;
  struct DNode *next;
} DNode;

Node* genlist(int length, int (*f)(int)){
  Node *root = malloc(sizeof(Node));
  Node *c = root;
  for(int i = 0; i < length; i++){
	Node *n = malloc(sizeof(Node));
	n->val = f(i);
	c->next = n;
	c = c->next;
	c->next = root->next;
  }
  return root;
}

int valgen(int i){
  return i; /* rand() % 100; */
}


  

int main(int argc, char **argv){
  int size = 10;
  Node* l = genlist(size, *valgen);
  Node* c = l->next;
  for(int i = 1; i <= 3*size; i++){
	printf("%i ", c->val);
	c = c->next;
	if(i % size == 0){ printf("\n"); }
  }
  printf("\n");
  return 0;
}
