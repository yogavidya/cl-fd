# cl-fd
A syntactic extension that allows to easily write safer Common Lisp backend code, with type checking and condition handling.
During the last five years, I found myself using too often similar patterns, like: 
* checking a function's arguments and return values against accepted types and validation functions,
* managing conditions to allow server-side code to run unattended

So I decided to automate these patterns. cl-fd allows to define so-called function descriptors - closures that store
all relevant information about a function, like its lambda list, argument types, validation functions for arguments,
types and validation functions for return values, documentation, name and body.

cl-instantiate allows in turn to actually create a function from a descriptor, selecting a name other than its default or
as an anonymous lambda if needed.

Work in progress.
