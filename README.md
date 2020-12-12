# elm-snake

A very simple classic "snake game" written in Elm to practice the key concepts.
Learned a bit about: rendering elements, subscribing to events, random generators etc.

Code is probably horrible because it's the first thing I made after a simple "todo app".

Bugs:
* You can't change direction to where you came from, except if you quickly input for example "down right" while heading "left".. In which case you will try to turn 180 degrees and die instantly
