
void main (int argc, char **argv) {
  Component p = $newComponent (initProducer, reactProducer);
  Component c = $newComponent (initConsumer, reactConsumer);
  List lis1 =listNewCell ();
  List lis2 =listNewCell ();
  List lis = $append (lis1, lis2);
  runQueue = lis;
$connect;
  Dispatcher ();
}

