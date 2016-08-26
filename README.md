#### tl;dr

This is a small command line utility for saving and restoring Visual Studio document window layouts across different Git branches.


### The Problem

Visual Studio saves information on open documents (along with other settings) in a `.suo` file per solution. As those settings are generally user-specific, it makes no sense to put the `.suo` in version control. This means that the open documents settings always stay the same when going back and forth between branches, leading to a (for me) annoying behavior:

- You work on a new feature in branch `A`. You have added new code files here and arranged your code tabs and windows in a way that is suitable for what you need to do.
- An issue pops up that needs to be fixed immediately. You checkout branch `B` from the main branch and get to work. Visual Studio tries to apply the previous document window layout, but the new files you added in branch `A` do not exist in branch `B`, so those tabs and windows can't be opened. The files that you had open which exist in this branch as well will be opened, but chances are you don't need those, so you'll either close or leave them, and open other files that are involved in the fix you need to make.
- You finish the fix in branch `B` and get back to branch `A`. The document settings stored in the `.suo` file are now those from your work in branch `B` - although your new branch `A` files are now present again, Visual Studio doesn't remember you had them open, and instead presents you with the files from your branch `B` fix that are not relevant to your work in branch `A`. You need to close those and then find your new branch `A` files again and restore your working layout by hand.

### The Sol.... Crutch

This is a small command line tool that can do one of two things:

- Extract the document window settings from a `.suo` file and remember them for the current Git branch
- Overwrite the document window settings in the `.suo` file with those previously stored for the current branch

If `bd.exe` is in `$PATH`, the workflow is as follows:

- If you need to switch branches, close the solution; this is necessary to make Visual Studio save the current window layout to the `.suo` file.
- Run `bd save` in the repository's root directory. This will look for any `.sln` files in the directory, try to find the respective `.suo` files and add or update the entry with the current branch's name in `<solutionname>.branchdocuments`.
- Checkout the other branch.
- Run `bd restore`. This will look for an entry with the current branch name in the `.branchdocuments` file, and if found, will replace the document settings in the `.suo` file with that data.
- Reopen the solution.

This is probably too cumbersome for most people to use but helps me in the short term. A much nicer solution would be a Visual Studio extension that takes care of this, but that would obviously be a lot more effort, and I don't know how feasible that really would be. 


### Limitations

This currently only works when running `bd.exe` in the directory that contains the `.sln` file.
 