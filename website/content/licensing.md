---
layout: page
title: Licensing
permalink: /licensing/
---

Open licensing of scientific material
=====================================

This memo gives recommendations for selecting open source license for
scientific material, including software, data and documents.  Let us
continue to make the newest computational techniques easily available
to spark new applications and further development!


Recommended open licenses for academic publishing
-------------------------------------------------


Software and source code
========================

The modified BSD licenses (in particular FreeBSD) and MIT license set minimal restrictions on the end user, and have therefore been recommended for academic purposes, for instance at ICML/MLOSS workshop 2010 ([V. Stodden](http://www.stanford.edu/~vcs/Papers.html) and G. Bradski), and in the [Quick Guide to Software Licensing for the Scientist-Programmer](http://www.ploscompbiol.org/article/info%3Adoi%2F10.1371%2Fjournal.pcbi.1002598) in PLoS CB. Also note that none of the standard licenses below excludes commercial use. Based on my experiences in developing scientific open source tools, I suggest the following preference order for selecting an open license:

* [FreeBSD](http://en.wikipedia.org/wiki/BSD_licenses#2-clause_license_.28.22Simplified_BSD_License.22_or_.22FreeBSD_License.22.29) (or other modified BSD licenses). Allows essentially free reuse of the code, and relicensing by others, assuming that the original open licensing statement is distributed with the code. Preferred over MIT since makes an explicit statement concerning binary versions of the code and contains a notice prohibiting the use of the name of the copyright holder in promotion. See also [other BSD licenses](http://en.wikipedia.org/wiki/BSD_licenses) that are slightly more restrictive than FreeBSD.
* [MIT license](http://en.wikipedia.org/wiki/MIT_License) Essentially similar to FreeBSD but slightly less explicit regarding binary versions of the code.
* [GPL (>=2)](http://www.gnu.org/licenses/old-licenses/gpl-2.0.txt) is more restrictive than FreeBSD or MIT licenses. One reason to license software under GPL occurs when your code contains parts of GPL-licensed code: the complete source code utilizing portions of GPL-licensed code needs to be released under GPL since this is a viral license. For compatibility, GPL(>=2) is often preferred over GPLv2 to allow end user select between GPLv2 and any later version. GPL should not the default choice for academic publishing since its requirements of viral distribution are somewhat incompatible with the general scientific standard of unrestricted reuse.  
* [LGPL](http://www.gnu.org/licenses/why-not-lgpl.html) requires that modified versions of your code are also published under LGPL, but not the whole software that utilizes the code. It is therefore less restrictive than GPL, but more restrictive than BSD or MIT that only require preservation of the license note in the code. See also [reasons not to use LGPL](http://www.gnu.org/licenses/why-not-lgpl.html).  
     
There are many [other open licenses](http://en.wikipedia.org/wiki/List_of_FSF_approved_software_licenses) but they may set restrictions that are not appropriate for academic purposes, or may have compatibility problems with other licenses. For instance, [Apache2.0](http://www.apache.org/licenses/) is nearly identical to FreeBSD and MIT, but [not always GPL2-compatible](http://stackoverflow.com/questions/40100/apache-licence-vs-bsd-vs-mit) which may prevent reuse (see also [this link](http://www.oss-watch.ac.uk/resources/opensourceyourcode.xml)). Many people tend to think that [GPLv3](http://www.gnu.org/licenses/gpl-3.0.html) sets too extensive restrictions, including requirements on hardware (see also [this](http://www.oreillynet.com/linux/blog/2007/06/linus_take_on_sun_opensolaris_1.html) and [this](http://www.oss-watch.ac.uk/resources/gpl3final.xml) for further discussion). '''Public domain''' or '''missing license''' do not imply open source and can prevent the reuse of your work since the concepts are legally ill-defined. '''Non-commercial clause''' is sometimes assigned to license (''not for commercial purposes''); note however that none of the standard open licenses sets such restrictions- commercial use of academic research results is not restricted in general, so why should source code be different? For further info, check [comparison of free software licenses](http://en.wikipedia.org/wiki/Comparison_of_free_software_licenses).



Data
====

* [CC-BY 4.0](http://creativecommons.org/licenses/by/4.0/) (Recommended for data)
* [CC0](http://wiki.creativecommons.org/CC0_FAQ) (Recommended for metadata; gives away all rights)
* [ODC-BY](http://www.opendatacommons.org/licenses/by/) Open Data Commons Attribution License (another open data license)
* [Public Domain Dedication and License (PDDL)](http://www.opendatacommons.org/about/) (another open data license)

See also the [Open Data Manual](http://opendatamanual.org/)


Documents
=========

* [Creative Commons](http://creativecommons.org/choose/)
* [GNU Free Documentation License](http://www.gnu.org/licenses/fdl.html)


Publishing your work
====================

The following repositories can be used to distribute
all research documents (data, code, publications, videos etc.). Github
is mostly for code; Figshare and Dryad are suited for data and
miscellaneous material:

 * [Github](http://github.org) (use primarily for code)
 * [Figshare](http://figshare.com)  
 * [Data Dryad](http://datadryad.org) (includes a fee)


You can also list your work in an appropriate repository for open
source content:

 * Software: [Machine Learning Open Source Software (MLOSS)](http://mloss.org/software/)
 * Data: [Public repositories for open data](http://oad.simmons.edu/oadwiki/Data_repositories)
 * Documents: [arXiv](http://arxiv.org); bioArXiv; etc.


Open licensing step-by-step 
---------------------------

1. Pick a standard license (see suggestions below)

1. Check [license compatibility](http://en.wikipedia.org/wiki/GNU_General_Public_License#Compatibility_and_multi-licensing) if your project includes external source code.

1. Ensure that you own sufficient copyrights for the licensing

1. Mention the license name in your documentation and source files,
and include the full license as a text file or provide a link.

1. Add your personal information (i.e. name, email, affiliation) in
the license.


Why license my work?
--------------------

Minimally restrictive licenses can help to promote the core scientific standards of publicity, transparency, reproducibility, and unrestricted use of research results. Motivations for open licensing include:

* Guarantee your own rights to your work
* Encourage the reuse of your work in a legally sustainable manner with minimal effort; missing licensing statements can prevent reuse
* Enforce core scientific standards of transparency and reproducibility (see [papers by V. Stodden](http://www.stanford.edu/~vcs/Papers.html))
* Valued by funding organizations, other scientists, fellow geeks, and laymen.
* It is simple 
* [Publish your computer code: it is good enough (Nature News)](http://www.nature.com/news/2010/101013/full/467753a.html)


Links and References
--------------------
* [A Quick Guide to Software Licensing for the Scientist-Programmer](http://www.ploscompbiol.org/article/info%3Adoi%2F10.1371%2Fjournal.pcbi.1002598); Morin/Urban/Sliz, PLoS CB 2012
* [Pohdintoja avoimen datan lisensoinnista Suomessa](http://julkinendata.posterous.com/#!/avoimen-datan-lisensointimalleja-maailmalta)
* Hietanen, Herkko: [The Pursuit of Efficient Copyright Licensing — How Some Rights Reserved Attempts to Solve the Problems of All Rights Reserved](http://urn.fi/URN:ISBN:978-952-214-721-9). Doctoral dissertation, 2008.
* Oksanen, Ville: [Five Essays on Copyright in the Digital Era](http://lib.tkk.fi/Diss/2008/isbn9789529983445/isbn9789529983445.pdf). Doctoral dissertation, 2008. 
* Stodden, Victoria: [Research on open standards for computational science](http://www.stanford.edu/~vcs/Papers.html)
* [opensource.com](http://www.opensource.com/)


