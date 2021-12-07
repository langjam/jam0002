
// producer
int counter;

void initProducer (Component self) {
  counter = 10;
}

void reactProducer (Component self, Message m) {
  counter -= 1;
  if (counter > 0) {
      $send (self, '*');
    } else {
      $withLock {
	systemRunning = 0;
      }
    }
}

