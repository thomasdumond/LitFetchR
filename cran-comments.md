## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


I have addressed the comment from Uwe Ligges from 22/12/2025, and declared where to get the 'ASySD' from in "Description". 

I have adressed the comments from Konstanze Lauseker from 07/01/2026, as follows:
- In the description, using undirected quotation marks only.
- Added value, their structure and output means in .Rd files, or precised when function had no returned value and are called for their side effects.
- Changed information messages using print()/cat() to message() so they can be easily silenced using suppressMessages().
- Modified the functions that were writing in the user's home filespace by adding the 'directory' argument so they choose where files are to be created. All getwd() have been removed.
- User's option are not being changed, setwd() have been removed.




