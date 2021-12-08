/* 
   This is part of the language jam - JAM0002.  
   See https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/doc/Plan.md
   This code implements a rudimentary multi-threaded system as described in the Call / Return Spaghetti article.
*/

#include <malloc.h>
#includ "kernel.h"

Cell* runQueue;

int systemRunning;

Connection connections [1];

int counter;



// consumer
void initConsumer (Component* self) {
  // nothing to do here
}

void reactConsumer (Component* self, Message* m) {
  printf ("consumer received: /%c/\n", (char) m->datum);
}

void main (int argc, char **argv) {
  Component* p = kernelNewComponent (initProducer, reactProducer);
  Component* c = kernelNewComponent (initConsumer, reactConsumer);
  ListCell* c2 = kernelNewListCell (c, NULL);
  ListCell* c1 = kernelNewListCell (p, c);
  runQueue = c1;
  connections [0]->sender = p;
  connections [0]->receiver = c;
  Dispatcher ();
}
