# About SSF
Ssf stands for "Simple Sound Font" and it is the name for a file type as well as the implementation of a library.
In the `specification.txt` file there is a very crude description of what a `ssf` file should look like.
The library implements a few useful functions for writing from and reading to a `ssf` file.
Yeah, yeah but what it is for? .. Well, as the name implies the file format is designed to store a collection of presumably small sound recordings.
It is as of now in no way complete or finished.

## Why Standard ML?
Because it's fun and it occurred to me that it could be a good language for the project.

# Goals
- Implement exception raising on invalid/bad arguments to functions
- Expand the specification to support other kinds of sample formats
- Expand the library to support the use of different samples formats, maybe even eliminating the need to work with the `sample` in a low level way.
