void DispatchTransferOutputsForOneComponent (Component c) {
  List outputs = $field (c, outputQueue);
  outputs = $reverse (outputs);
  while (!$isEmpty (outputs)) {
    Component receiver = $connectedTo (c);
    Datum data = $data (outputs);
    Message m = $newMessage (data, NULL);
    $appendInput (receiver, m);
    $rest (outputs);
  }
}

void DispatchOnce () {
  List all = runQueue;
  while (!$isEmpty (all) {
      component = $data (all);
      if ($hasInputs (component)) {
	Message m = $popInput (component);
	$callReaction (component, m);
      }
      $rest (all);
    }
}
  
void Dispatcher () {
  while (systemRunning) {
    DispatchOnce ();
  }
  DispatchTransferOutputs ();
}

void DispatchTransferOutputs () {
  List all = runQueue;
  c = $first (all);
  while (!$isEmpty (all)) {
    DispatchTransferOutputsForOneComponent (componentList);
    $rest (all);
  }
}

