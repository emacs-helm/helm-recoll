helm-recoll
===========

helm interface for the recoll desktop search tool.


Commentary:
===========

You need to create some helm-recoll sources before you can use them.
You can create sources using the `helm-recoll-create-source' function,
e.g. like this:

```elisp
(helm-recoll-create-source "docs" "~/.recoll/docs")
(helm-recoll-create-source "progs" "~/.recoll/progs")
```

Then you can use the sources in helm like this:

```elisp
(helm :sources '(helm-source-recoll-docs helm-source-recoll-progs :buffer *helm recoll*))
```

Installation:
=============

Add code to your init (~/.emacs) file to create some sources (see above),
and then add a require statement for the library:

```elisp
(require 'helm-recoll)
```
