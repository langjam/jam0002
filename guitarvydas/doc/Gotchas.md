- isReady is slightly more complicated when Components can be containers (isReady is true only if the container as input messages queued up AND if all of its children are not busy)

- append() and append1() could use DRY.  DRY (or DRY detection) should be automated and not a burden for the programmer to bear.
