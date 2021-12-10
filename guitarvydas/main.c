#include "mpos.h"
#include "producer.h"
#include "consumer.h"
#include "kernel.h"

int main (int argc, char **argv) {
  Component p =  componentNew (initProducer, reactProducer) ;
  Component c =  componentNew (initConsumer, reactConsumer) ;
  ListCell lis1 = listNewCellComponent (p);
  ListCell lis2 = listNewCellComponent (c);
lis1 = listAppend (lis1, lis2);
  runQueue = lis1;
connectionsConnect (p, c);
  Dispatcher ();
  return 0;
}

