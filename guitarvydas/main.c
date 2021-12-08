
void main (int argc, char **argv) {
  Component p =  componentNew (initProducer, reactProducer) ;
  Component c =  componentNew (initConsumer, reactConsumer) ;
  List lis1 = listNewCell (p);
  List lis2 = listNewCell (c);
  List lis = lis1 = listAppend1 (lis1, lis2);
  runQueue = lis;
connectionsConnect (p, c);
  Dispatcher ();
}

