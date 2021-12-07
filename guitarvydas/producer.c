
// producer
int counter;

void initProducer (Component self) {
  $initializeCounter
}

void reactProducer (Component self, Message m) {
  $decCounter
  if ($counterIsGreaterThanZero) {
      $send (self, '*');
    } else {
      $withLock {
	systemRunning = 0;
      }
    }
}

