
// producer
int counter;

void initProducer (Component self) {
counter = 0;
}

void reactProducer (Component self, Message m) {
counter -= 1;
  if (counter > 0) {
$send;
    } else {
$withLock{
	systemRunning = 0;
      }
}
}

