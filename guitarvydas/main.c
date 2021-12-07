
void main (int argc, char **argv) {
  Component p = $newComponent;
  Component c = $newComponent;
  List lis1 = listNewCell (p);
  List lis2 = listNewCell (c);
  List lis = $append (lis1, lis2);
  runQueue = lis;
$connect;
  Dispatcher ();
}

