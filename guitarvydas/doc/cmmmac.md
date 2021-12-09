idea(s) for possible cmm spec file (incomplete):

```
// mechanism List
$first" 1 -
$reverse" 1 -
$rest" 1 -
$isEmpty" 1 -
$newListCell" 1 -
$append" 1 -

// mechanism Component
$newComponent" 2 -
$field" 1 -
$appendInput" 2 -
$popInput" 1 -
$callReaction" 2 -
$hasInputs" 1 -

// mechanism Message
$newMessage" 1 -
$data" 1 -

// mechanism ConnectionTable
$connectedTo" 1 -
$connect" 2 -

// mechanism Kernel
$withLock" block -
$send" 2 -
$panic" 1 -

// mechanism Counter
$initializeCounter" 0 -
$decCounter" 0 -
$counterIsGreaterThanZero" 0 -
```