# WPF_lab

## Arytmetyka
The main task was to implement functions such as add, subtract, multiply, divide (and other less demanding such as min, max, etc.) on intervals.

## Leftist
The task was to implement leftist priority queue.

## iSet
The task was to implement operations on sets of integers. To make the computations time optimized they are kept in AVL-tree data structure. \
Example: 
* We have 1 set of integers [10, 20]
* Then we subtract set [13, 17] so we are left with sets [10, 12] and [18, 20]
* Then we add again set [11, 19] which overlaps with our two sets so we need to merge them and we are left again with a set of [10, 20]
  
## Origami
The main goal of a task was to implement function to fold an imaginary sheet of paper (once or multiple times) and to answer how many layers of paper are stacked at a given point.
  
## Topol
The task was pretty straightforward to sort DAG topologically.

## Przelewanka
Algorithmic task where you are given n empty glasses of volume _x1, x2, ..., xn_ return if it is possible to end in situation where there is _y1, y2, ..., yn_ water in glasses _1, 2, ..., n_, while having only 3 operations
* fill the glass to _xi_
* empty the glass
* pour the water from glass _i_ to glass _j_ (the remaining water stays in the glass _i_)

This is the "Mokra robota" task from the III Polish Olympiad in Informatics.
