#include "mpos.h"
#include "producer.h"
#include "consumer.h"
#include "kernel.h"

int main (int argc, char **argv) {
  Component* p =  componentNew (initProducer, reactProducer) ;
  Component* c =  componentNew (initConsumer, reactConsumer) ;
  List* lis1 = listNewCellComponent (p);
  List* lis2 = listNewCellComponent (c);
append (lis1, lis2);
  runQueue = lis1;
connectionsConnect (p, c);
  Dispatcher ();
  return 0;
}

