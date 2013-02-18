
The API is currently designed only for theorem prover invocations that either
time out or answer Unsatisfiable. I would like to add support when they answer
Satisfiable. I am not sure if the tree model would fit for the cancellation
properties, or if a graph should be introduced.

A timeout of 0ms causes a deadlock.

Implement evalTree using TMVars.

QuickCheck tests of external processes.

Better interleaving of processes.

Add a Running case.

Record and store the execution time of processes.

