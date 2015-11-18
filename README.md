helm-recoll
===========

helm interface for the recoll desktop search tool.


Commentary:
===========

You need to create some helm-recoll sources before you can use them.
You can create sources using the `helm-recoll-create-source' macro,
e.g. like this:

```elisp
(helm-recoll-create-source "docs" "~/.recoll/docs")
(helm-recoll-create-source "progs" "~/.recoll/progs")
```

`helm-recoll-create-source' defines a source named
helm-source-recoll-NAME' and an individual command named
`helm-recoll-NAME' for every defined source.  With the above
definitions, the commands `helm-recoll-docs' and `helm-recoll-progs'
would be provided and start a Helm session for the created
`helm-source-recoll-docs' and `helm-source-recoll-progs' sources
respectivly.

In addition, the command `helm-recoll' lists all currently defined
recoll sources, and starts a helm session running the selected ones.


Installation:
=============

Add a require statement for the library:

```elisp
(require 'helm-recoll)
```

to your init file (typically ~/.emacs) and add your source definitions
like shown above.
