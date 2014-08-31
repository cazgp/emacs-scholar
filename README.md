Emacs-Scholar
=============

Requires
========

* zotero
* emacs
* zotelo (emacs)
* reftex (emacs)
* reftex-cite (emacs)


Usage
=====

* Add the .el to emacs config.
* Create a file `.org` and another `.bib`.
* In the `.bib` file add a comment at the end `\bibliography{filename.bib}`. Restart org and the file.
* Adds the command `C-c )`. This will hook up to your zotero and ask which collection you would like to associate with. Choose the collection with the arrow keys. A new org-heading will be created as the reference.
* Write notes under the heading.
* Anytime you change the zotero collection and run `C-c )` the `filename.bib` will be updated.


To Do
=====

* If the filename changes in the bib file that change isn't propagated throughout `.org` so dead links may be created. Find a way to fix this.
