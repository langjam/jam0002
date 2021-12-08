
void main (int argc, char **argv) {
  Component p = $newComponent (initProducer, reactProducer);
  Component c = $newComponent (initConsumer, reactConsumer);
  List lis1 = $newListCell (p);
  List lis2 = $newListCell (c);
  List lis = $append (lis1, lis2);
  runQueue = lis;
  $connect (p, c);
  Dispatcher ();
}
